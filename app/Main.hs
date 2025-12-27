{-# LANGUAGE ApplicativeDo #-}

-- | Synapse CLI - Algebraic Implementation
--
-- This executable uses the categorical machinery:
-- - Effect stack (SynapseM) with caching and cycle detection
-- - Reified algebras for navigation, rendering, completion
-- - Proper error handling
--
-- Compare with Main.hs which takes pragmatic shortcuts.
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, hFlush, stdout)

import Plexus.Types (PlexusStreamItem(..))

import Synapse.Schema.Types
import Synapse.Monad
import Synapse.Algebra.Navigate
import Synapse.Algebra.Render (renderSchema, renderMethodFull)
import Synapse.Transport

-- ============================================================================
-- Types
-- ============================================================================

data Args = Args
  { argHost    :: Text
  , argPort    :: Int
  , argJson    :: Bool
  , argDryRun  :: Bool
  , argSchema  :: Bool         -- ^ Show raw schema JSON
  , argParams  :: Maybe Text   -- ^ JSON params via -p
  , argRpc     :: Maybe Text   -- ^ Raw JSON-RPC passthrough
  , argPath    :: [Text]       -- ^ Path segments and --key value params
  }
  deriving Show

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- execParser argsInfo
  env <- initEnv (argHost args) (argPort args)
  result <- runSynapseM env (dispatch args)
  case result of
    Left err -> do
      hPutStrLn stderr $ renderError err
      exitFailure
    Right () -> exitSuccess

-- | Dispatch based on navigation result
dispatch :: Args -> SynapseM ()
dispatch Args{..} = do
  -- Mode 1: Raw JSON-RPC passthrough
  case argRpc of
    Just rpcJson -> do
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 rpcJson) of
        Left err -> throwParse $ T.pack err
        Right rpcReq -> do
          items <- invokeRawRpc rpcReq
          liftIO $ mapM_ (printResult argJson) items
      return ()

    Nothing -> do
      -- Parse path and inline params (--key value pairs)
      let (pathSegs, inlineParams) = parsePathAndParams argPath

      -- Mode 2: Schema request
      if argSchema
        then do
          items <- invokeRaw (buildSchemaMethod pathSegs) (object [])
          liftIO $ mapM_ (printResult True) items  -- Always JSON for schema
        else do
          -- Mode 3: Normal navigation
          if null pathSegs
            then do
              rootSchema <- navigate []
              case rootSchema of
                ViewPlugin schema _ -> liftIO $ do
                  TIO.putStr cliHeader
                  TIO.putStr "\n\n"
                  TIO.putStr $ renderSchema schema
                _ -> pure ()
            else do
              -- Navigate to target
              view <- navigate pathSegs
              case view of
                -- Landed on a plugin: show help
                ViewPlugin schema _ ->
                  liftIO $ TIO.putStr $ renderSchema schema

                -- Landed on a method: invoke or show help
                ViewMethod method path -> do
                  -- Build params: -p JSON takes precedence, then inline --key value
                  params <- case argParams of
                    Just jsonStr ->
                      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                        Left err -> throwParse $ T.pack err
                        Right p -> pure p
                    Nothing
                      | not (null inlineParams) -> pure $ buildParamsObject inlineParams
                      | otherwise -> pure $ object []

                  if argDryRun
                    then liftIO $ LBS.putStrLn $ encodeDryRun (init path) (last path) params
                    else if hasRequiredParams method && params == object [] && null inlineParams
                      then liftIO $ TIO.putStrLn $ renderMethodFull method
                      else invokeMethod path params
  where
    invokeMethod path params = do
      let namespacePath = init path  -- path without method name
      let methodName' = last path
      items <- invoke namespacePath methodName' params
      liftIO $ mapM_ (printResult argJson) items

    -- Check if method has required parameters
    hasRequiredParams :: MethodSchema -> Bool
    hasRequiredParams m = case methodParams m of
      Nothing -> False
      Just (Object o) -> case KM.lookup "required" o of
        Just (Array arr) -> not (null arr)
        _ -> False
      Just _ -> False

    -- Build schema method path
    buildSchemaMethod :: [Text] -> Text
    buildSchemaMethod [] = "plexus.schema"
    buildSchemaMethod segs = T.intercalate "." segs <> ".schema"

    -- Invoke raw JSON-RPC request
    invokeRawRpc :: Value -> SynapseM [PlexusStreamItem]
    invokeRawRpc rpcReq = do
      case rpcReq of
        Object o -> case (KM.lookup "method" o, KM.lookup "params" o) of
          (Just (String method), Just params) -> invokeRaw method params
          (Just (String method), Nothing) -> invokeRaw method (object [])
          _ -> throwParse "JSON-RPC must have 'method' field"
        _ -> throwParse "JSON-RPC must be an object"

-- | Parse path segments and inline key=value params
-- Returns (path segments, [(key, value)] params)
-- Example: ["echo", "once", "message=hello", "count=3"]
--       -> (["echo", "once"], [("message", "hello"), ("count", "3")])
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)])
parsePathAndParams = go [] []
  where
    go path params [] = (reverse path, reverse params)
    go path params (x:xs)
      | Just (k, v) <- parseKeyValue x =
          go path ((k, v) : params) xs
      | otherwise =
          go (x : path) params xs

    -- Parse "key=value" into Just (key, value), or Nothing
    parseKeyValue :: Text -> Maybe (Text, Text)
    parseKeyValue t = case T.breakOn "=" t of
      (k, rest)
        | not (T.null rest) && not (T.null k) ->
            Just (k, T.drop 1 rest)  -- drop the "="
        | otherwise -> Nothing

-- | Build JSON object from key-value pairs
buildParamsObject :: [(Text, Text)] -> Value
buildParamsObject pairs = object
  [ (K.fromText k, inferValue v) | (k, v) <- pairs ]
  where
    -- Try to infer the JSON type from the string value
    inferValue :: Text -> Value
    inferValue t
      | t == "true" = Bool True
      | t == "false" = Bool False
      | Just n <- readMaybe (T.unpack t) :: Maybe Integer = Number (fromInteger n)
      | Just n <- readMaybe (T.unpack t) :: Maybe Double = Number (realToFrac n)
      | otherwise = String t

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- | ASCII splash logo
splash :: Text
splash = T.unlines
  [ ""
  , "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
  , ""
  ]

-- | Get CLI help text from optparse-applicative
cliHeader :: Text
cliHeader = splash <> T.pack (fst $ renderFailure failure "synapse")
  where
    failure = parserFailure defaultPrefs argsInfo (ShowHelpText Nothing) mempty

-- | Render an error for display
renderError :: SynapseError -> String
renderError = \case
  NavError (NotFound seg path) ->
    "Not found: '" <> T.unpack seg <> "' at " <> showPath path
  NavError (MethodNotTerminal seg path) ->
    "Method '" <> T.unpack seg <> "' cannot have subcommands at " <> showPath path
  NavError (Cycle hash path) ->
    "Cycle detected: hash " <> T.unpack hash <> " at " <> showPath path
  NavError (FetchError msg path) ->
    "Fetch error at " <> showPath path <> ": " <> T.unpack msg
  TransportError msg ->
    "Transport error: " <> T.unpack msg
  ParseError msg ->
    "Parse error: " <> T.unpack msg
  ValidationError msg ->
    "Validation error: " <> T.unpack msg
  where
    showPath [] = "root"
    showPath p = T.unpack $ T.intercalate "." p

-- | Encode a dry-run request
encodeDryRun :: [Text] -> Text -> Value -> LBS.ByteString
encodeDryRun namespacePath method params =
  let fullPath = if null namespacePath then ["plexus"] else namespacePath
      dotPath = T.intercalate "." (fullPath ++ [method])
  in encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= ("plexus_call" :: Text)
    , "params" .= object
        [ "method" .= dotPath
        , "params" .= params
        ]
    ]

-- | Print a stream result
printResult :: Bool -> PlexusStreamItem -> IO ()
printResult True item = LBS.putStrLn $ encode item
printResult False item = case item of
  StreamData _ _ _ dat -> do
    TIO.putStrLn $ TE.decodeUtf8 $ LBS.toStrict $ encode dat
    hFlush stdout
  StreamProgress _ _ msg _ -> do
    TIO.putStr msg
    TIO.putStr "\r"
    hFlush stdout
  StreamError _ _ err _ ->
    hPutStrLn stderr $ "Error: " <> T.unpack err
  _ -> pure ()

-- ============================================================================
-- Argument Parsing
-- ============================================================================

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "synapse - Algebraic CLI for Plexus"
 <> progDesc "Navigate and invoke methods via coalgebraic schema traversal"
  )

argsParser :: Parser Args
argsParser = do
  argHost <- T.pack <$> strOption
    ( long "host" <> short 'H' <> metavar "HOST"
   <> value "127.0.0.1" <> showDefault
   <> help "Plexus server host" )
  argPort <- option auto
    ( long "port" <> short 'P' <> metavar "PORT"
   <> value 4444 <> showDefault
   <> help "Plexus server port" )
  argJson <- switch
    ( long "json" <> short 'j'
   <> help "Output raw JSON stream" )
  argDryRun <- switch
    ( long "dry-run" <> short 'n'
   <> help "Show JSON-RPC request without sending" )
  argSchema <- switch
    ( long "schema" <> short 's'
   <> help "Fetch raw schema JSON for path" )
  argParams <- optional $ T.pack <$> strOption
    ( long "params" <> short 'p' <> metavar "JSON"
   <> help "Method parameters as JSON object" )
  argRpc <- optional $ T.pack <$> strOption
    ( long "rpc" <> short 'r' <> metavar "JSON"
   <> help "Raw JSON-RPC request (bypass navigation)" )
  argPath <- many $ T.pack <$> argument str
    ( metavar "PATH... [key=value ...]"
   <> help "Path to plugin/method, with optional key=value params" )
  pure Args{..}
