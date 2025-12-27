{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

-- | Synapse CLI - Schema-driven interface to Plexus
--
-- = Design
--
-- The CLI discovers commands at runtime from the schema coalgebra:
-- - Path segments are positional arguments
-- - Schema is fetched lazily as we navigate
-- - Methods at the target path become available commands
--
-- = Examples
--
-- @
-- synapse                          # show root help
-- synapse echo                     # show echo help
-- synapse echo once --message hi   # invoke echo.once
-- synapse solar earth info         # invoke solar.earth.info
-- @
module Main where

import Control.Exception (SomeException, catch)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Options.Applicative
import qualified Streaming.Prelude as S
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (PlexusConfig(..), plexusRpc)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema.Recursive

-- ============================================================================
-- Types
-- ============================================================================

data GlobalOpts = GlobalOpts
  { optHost    :: String
  , optPort    :: Int
  , optJson    :: Bool
  , optDryRun  :: Bool
  , optParams  :: Maybe Text  -- ^ Raw JSON params
  } deriving Show

-- | Command result from parsing
data Command
  = CmdInvoke [Text] Text Value   -- ^ path (namespaces), method, params
  | CmdShowHelp [Text]            -- ^ show help for path
  deriving Show

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  -- Parse basic args first (path and global opts)
  (global, pathSegs) <- execParser basicParserInfo

  -- Fetch schema for the path
  schemaResult <- fetchSchemaForPath global pathSegs
  case schemaResult of
    Left err -> do
      hPutStrLn stderr $ "Error: " <> T.unpack err
      exitFailure
    Right (schema, remainingPath) ->
      -- The namespace path is pathSegs minus remainingPath
      let namespacePath = take (length pathSegs - length remainingPath) pathSegs
      in case remainingPath of
        [] -> do
          -- No remaining path - show help for this schema
          TIO.putStr $ renderSchema schema
        [methodName] ->
          -- Single remaining segment - might be a method
          case findMethod methodName schema of
            Just method -> do
              -- Parse method params from -p flag or empty object
              params <- case optParams global of
                Just jsonStr -> case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                  Right obj -> pure obj
                  Left err -> do
                    hPutStrLn stderr $ "Invalid JSON params: " <> err
                    exitFailure
                Nothing -> pure $ object []
              runInvoke global namespacePath methodName params
            Nothing -> do
              -- Not a method - show error
              hPutStrLn stderr $ "Unknown method or namespace: " <> T.unpack methodName
              TIO.putStr $ renderSchema schema
              exitFailure
        (seg:_) -> do
          -- Multiple remaining segments but we couldn't navigate
          hPutStrLn stderr $ "Unknown namespace: " <> T.unpack seg
          TIO.putStr $ renderSchema schema
          exitFailure

-- | Find a method by name in a schema
findMethod :: Text -> PluginSchema -> Maybe MethodSchema
findMethod name schema =
  case filter ((== name) . methodName) (psMethods schema) of
    [m] -> Just m
    _   -> Nothing

-- ============================================================================
-- Argument Parsing
-- ============================================================================

basicParserInfo :: ParserInfo (GlobalOpts, [Text])
basicParserInfo = info (parser <**> helper)
  ( fullDesc
 <> header "synapse - CLI for Plexus"
 <> progDesc "Navigate and invoke Plexus methods"
  )
  where
    parser = (,) <$> globalParser <*> pathParser

-- | Global options
globalParser :: Parser GlobalOpts
globalParser = GlobalOpts
  <$> strOption
      ( long "host" <> short 'H' <> metavar "HOST"
     <> value "127.0.0.1" <> showDefault
     <> help "Plexus server host" )
  <*> option auto
      ( long "port" <> short 'P' <> metavar "PORT"
     <> value 4444 <> showDefault
     <> help "Plexus server port" )
  <*> switch
      ( long "json" <> short 'j'
     <> help "Output raw JSON stream" )
  <*> switch
      ( long "dry-run" <> short 'n'
     <> help "Show JSON-RPC request without sending" )
  <*> optional (strOption
      ( long "params" <> short 'p' <> metavar "JSON"
     <> help "Method parameters as JSON object" ))

-- | Path segments as positional arguments
pathParser :: Parser [Text]
pathParser = many $ argument (T.pack <$> str)
  ( metavar "PATH..."
 <> help "Path to plugin/method (e.g., solar earth info)" )

-- ============================================================================
-- Schema Navigation
-- ============================================================================

-- | Fetch schema for a path, navigating through children
-- Returns the deepest schema we could reach and any remaining path segments
fetchSchemaForPath :: GlobalOpts -> [Text] -> IO (Either Text (PluginSchema, [Text]))
fetchSchemaForPath opts path = navigatePath opts [] path

-- | Navigate through schemas, fetching children as needed
navigatePath :: GlobalOpts -> [Text] -> [Text] -> IO (Either Text (PluginSchema, [Text]))
navigatePath opts visited [] = do
  -- Fetch schema at current position
  schemaResult <- fetchSchemaAt opts visited
  case schemaResult of
    Left err -> pure $ Left err
    Right schema -> pure $ Right (schema, [])
navigatePath opts visited (seg:rest) = do
  -- Fetch schema at current position
  schemaResult <- fetchSchemaAt opts visited
  case schemaResult of
    Left err -> pure $ Left err
    Right schema ->
      -- Check if seg is a child namespace
      if seg `elem` childNamespaces schema
        then navigatePath opts (visited ++ [seg]) rest
        else
          -- seg is not a child - might be a method or error
          -- Return current schema with remaining path
          pure $ Right (schema, seg:rest)

-- | Fetch schema at a specific path
-- Uses plexus_call to route to the appropriate schema method
fetchSchemaAt :: GlobalOpts -> [Text] -> IO (Either Text PluginSchema)
fetchSchemaAt opts path = do
  let cfg = defaultConfig { plexusHost = optHost opts, plexusPort = optPort opts }
  -- Build the schema method path: plexus.schema, echo.schema, solar.earth.schema, etc.
  let schemaMethod = if null path
        then "plexus.schema"
        else T.intercalate "." path <> ".schema"
  let callParams = object ["method" .= schemaMethod]
  doFetch cfg callParams `catch` \(e :: SomeException) ->
    pure $ Left $ T.pack $ "Connection error: " <> show e
  where
    doFetch cfg params = do
      conn <- connect cfg
      mSchema <- S.head_ $ S.mapMaybe extractPluginSchema $
        plexusRpc conn "plexus_call" params
      disconnect conn
      case mSchema of
        Just (Right s) -> pure $ Right s
        Just (Left e)  -> pure $ Left e
        Nothing        -> pure $ Left "No schema received"

extractPluginSchema :: PlexusStreamItem -> Maybe (Either Text PluginSchema)
extractPluginSchema (StreamData _ _ ct dat)
  -- Content type ends with ".schema" (e.g., "plexus.schema", "echo.schema", "solar.earth.schema")
  | ".schema" `T.isSuffixOf` ct = Just $ parsePluginSchema dat
  | otherwise = Nothing
extractPluginSchema (StreamError _ _ err _) = Just (Left err)
extractPluginSchema _ = Nothing

-- ============================================================================
-- Command Execution
-- ============================================================================

runInvoke :: GlobalOpts -> [Text] -> Text -> Value -> IO ()
runInvoke opts pathSegs method params = do
  -- Build the full method path: path + method
  -- For root level, prefix with "plexus"
  let fullPath = if null pathSegs then ["plexus"] else pathSegs
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  let rpcRequest = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "method" .= ("plexus_call" :: Text)
        , "params" .= callParams
        ]
  if optDryRun opts
    then LBS.putStrLn $ encode rpcRequest
    else do
      let cfg = defaultConfig { plexusHost = optHost opts, plexusPort = optPort opts }
      conn <- connect cfg
      S.mapM_ (printResult opts) $ plexusRpc conn "plexus_call" callParams
      disconnect conn

printResult :: GlobalOpts -> PlexusStreamItem -> IO ()
printResult opts item
  | optJson opts = LBS.putStrLn $ encode item
printResult _ (StreamData _ _ _ dat) = do
  TIO.putStrLn $ formatJson dat
  hFlush stdout
printResult _ (StreamProgress _ _ msg _) = do
  TIO.putStr msg
  TIO.putStr "\r"
  hFlush stdout
printResult _ (StreamError _ _ err _) =
  hPutStrLn stderr $ "Error: " <> T.unpack err
printResult _ _ = pure ()

formatJson :: Value -> Text
formatJson = TE.decodeUtf8 . LBS.toStrict . encode

-- ============================================================================
-- Schema Rendering
-- ============================================================================

-- | Render schema to text - shows current node's methods and child namespaces
renderSchema :: PluginSchema -> Text
renderSchema schema = T.unlines $
  [ ""
  , psNamespace schema <> " - " <> psDescription schema
  , ""
  ] <>
  (if null (psMethods schema) then [] else
    [ "Methods:"
    ] <> map renderMethod (sortOn methodName $ psMethods schema) <> [""]
  ) <>
  (if null (pluginChildren schema) then [] else
    [ "Namespaces:"
    ] <> map renderChildSummary (sortOn csNamespace $ pluginChildren schema)
  )

renderMethod :: MethodSchema -> Text
renderMethod m =
  "  " <> padRight 16 (methodName m) <> methodDescription m <> renderParams (methodParams m)

-- | Render method parameters from JSON Schema
renderParams :: Maybe Value -> Text
renderParams Nothing = ""
renderParams (Just (Object o)) = case KM.lookup "properties" o of
  Just (Object props) ->
    let reqList = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- foldr (:) [] arr]
          _ -> []
        propList = KM.toList props
        -- Sort: required first, then alphabetically
        sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
        rendered = map (renderParam reqList) sorted
    in if null rendered then "" else "\n" <> T.intercalate "\n" rendered
  _ -> ""
renderParams _ = ""

-- | Render a single parameter
renderParam :: [Text] -> (K.Key, Value) -> Text
renderParam required (name, propSchema) =
  let nameText = K.toText name
      isReq = nameText `elem` required
      flagName = T.replace "_" "-" nameText
      (typ, desc) = extractTypeDesc propSchema
      reqMarker = if isReq then "" else "?"
  in "      --" <> flagName <> reqMarker <> " <" <> typ <> ">  " <> desc

extractTypeDesc :: Value -> (Text, Text)
extractTypeDesc (Object po) =
  ( case KM.lookup "type" po of { Just (String t) -> t; _ -> "string" }
  , case KM.lookup "description" po of { Just (String d) -> d; _ -> "" }
  )
extractTypeDesc _ = ("string", "")

renderChildSummary :: ChildSummary -> Text
renderChildSummary child =
  "  " <> padRight 16 (csNamespace child) <> csDescription child

padRight :: Int -> Text -> Text
padRight n t
  | T.length t >= n = t <> " "
  | otherwise = t <> T.replicate (n - T.length t) " "
