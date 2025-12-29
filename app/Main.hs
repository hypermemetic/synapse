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

import Plexus (PlexusStreamItem(..))

import Synapse.Schema.Types
import Synapse.Monad
import Synapse.Algebra.Navigate
import Synapse.Algebra.Render (renderSchema, renderMethodFull)
import Synapse.Algebra.TemplateGen (GeneratedTemplate(..), generateAllTemplatesWithCallback)
import Synapse.Transport
import Synapse.Renderer (RendererConfig, defaultRendererConfig, renderItem, prettyValue)
import System.Directory (createDirectoryIfMissing)

-- ============================================================================
-- Types
-- ============================================================================

data Args = Args
  { argHost      :: Text
  , argPort      :: Int
  , argJson      :: Bool          -- ^ Output raw JSON stream items
  , argRaw       :: Bool          -- ^ Output raw content (no templates)
  , argDryRun    :: Bool
  , argSchema    :: Bool          -- ^ Show raw schema JSON
  , argGenerate  :: Bool          -- ^ Generate templates from schemas
  , argParams    :: Maybe Text    -- ^ JSON params via -p
  , argRpc       :: Maybe Text    -- ^ Raw JSON-RPC passthrough
  , argPath      :: [Text]        -- ^ Path segments and --key value params
  }
  deriving Show

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- execParser argsInfo
  env <- initEnv (argHost args) (argPort args)
  rendererCfg <- defaultRendererConfig
  result <- runSynapseM env (dispatch args rendererCfg)
  case result of
    Left err -> do
      hPutStrLn stderr $ renderError err
      exitFailure
    Right () -> exitSuccess

-- | Dispatch based on navigation result
dispatch :: Args -> RendererConfig -> SynapseM ()
dispatch Args{..} rendererCfg = do
  -- Mode 1: Raw JSON-RPC passthrough
  case argRpc of
    Just rpcJson -> do
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 rpcJson) of
        Left err -> throwParse $ T.pack err
        Right rpcReq -> do
          items <- invokeRawRpc rpcReq
          liftIO $ mapM_ (printResult argJson argRaw rendererCfg) items
      return ()

    Nothing -> do
      -- Parse path and inline params (--key value pairs)
      let (pathSegs, inlineParams) = parsePathAndParams argPath

      -- Mode 2: Schema request
      if argSchema
        then do
          -- Try to navigate and determine if last segment is a method
          schemaResult <- fetchSchemaForPath pathSegs
          case schemaResult of
            Left err -> throwNav $ FetchError err pathSegs
            Right val -> liftIO $ LBS.putStrLn $ encode val

        -- Mode 3: Generate templates
        else if argGenerate
        then do
          let baseDir = ".substrate/templates"
              writeAndLog gt = do
                writeGeneratedTemplate baseDir gt
                TIO.putStrLn $ "  " <> T.pack (gtPath gt)
          liftIO $ TIO.putStrLn $ "Generating templates in " <> T.pack baseDir <> "..."
          count <- generateAllTemplatesWithCallback writeAndLog pathSegs
          liftIO $ TIO.putStrLn $ "Generated " <> T.pack (show count) <> " templates"

        else do
          -- Mode 4: Normal navigation
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
      invokeStreaming namespacePath methodName' params (printResult argJson argRaw rendererCfg)

    -- Check if method has required parameters
    hasRequiredParams :: MethodSchema -> Bool
    hasRequiredParams m = case methodParams m of
      Nothing -> False
      Just (Object o) -> case KM.lookup "required" o of
        Just (Array arr) -> not (null arr)
        _ -> False
      Just _ -> False

    -- Fetch schema for a path, detecting if last segment is a method
    -- Uses navigate to determine what type of schema to fetch
    fetchSchemaForPath :: [Text] -> SynapseM (Either Text Value)
    fetchSchemaForPath segs = do
      view <- navigate segs
      case view of
        ViewPlugin schema _ -> pure $ Right $ toJSON schema
        ViewMethod method path -> do
          -- Use method-specific schema query for detailed method info
          let parentPath = init path
              methodName' = last path
          detailedMethod <- fetchMethodSchema parentPath methodName'
          pure $ Right $ toJSON detailedMethod

    -- Invoke raw JSON-RPC request
    invokeRawRpc :: Value -> SynapseM [PlexusStreamItem]
    invokeRawRpc rpcReq = do
      case rpcReq of
        Object o -> case (KM.lookup "method" o, KM.lookup "params" o) of
          (Just (String method), Just params) -> invokeRaw method params
          (Just (String method), Nothing) -> invokeRaw method (object [])
          _ -> throwParse "JSON-RPC must have 'method' field"
        _ -> throwParse "JSON-RPC must be an object"

-- | Parse path segments and --key value params
-- Returns (path segments, [(key, value)] params)
-- Example: ["echo", "once", "--message", "hello", "--count", "3"]
--       -> (["echo", "once"], [("message", "hello"), ("count", "3")])
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)])
parsePathAndParams = go [] []
  where
    go path params [] = (reverse path, reverse params)
    go path params (x:xs)
      -- --key value pair
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs =
          go path ((key, val) : params) rest
      -- --key with no value (skip malformed)
      | T.isPrefixOf "--" x =
          go path params xs
      -- Regular path segment
      | otherwise =
          go (x : path) params xs

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

-- | Write a generated template to disk (no logging)
writeGeneratedTemplate :: FilePath -> GeneratedTemplate -> IO ()
writeGeneratedTemplate baseDir gt = do
  let fullPath = baseDir </> gtPath gt
  createDirectoryIfMissing True (baseDir </> T.unpack (gtNamespace gt))
  TIO.writeFile fullPath (gtTemplate gt)
  where
    (</>) = \a b -> a ++ "/" ++ b

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
-- argJson: output raw JSON stream items
-- argRaw: skip template rendering, just output content JSON
-- otherwise: try template rendering, fall back to content JSON
printResult :: Bool -> Bool -> RendererConfig -> PlexusStreamItem -> IO ()
printResult True _ _ item = LBS.putStrLn $ encode item
printResult _ True _ item = case item of
  -- Raw mode: just output the content
  StreamData _ _ _ dat -> do
    LBS.putStrLn $ encode dat
    hFlush stdout
  StreamProgress _ _ msg _ -> do
    TIO.putStr msg
    TIO.putStr "\r"
    hFlush stdout
  StreamError _ _ err _ ->
    hPutStrLn stderr $ "Error: " <> T.unpack err
  _ -> pure ()
printResult _ _ cfg item = do
  -- Template mode: try to render with template
  mRendered <- renderItem cfg item
  case mRendered of
    Just text -> do
      TIO.putStrLn text
      hFlush stdout
    Nothing -> case item of
      -- Fallback to pretty-printed content
      StreamData _ _ _ dat -> do
        TIO.putStrLn $ prettyValue dat
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
 <> forwardOptions  -- Pass unrecognized --flags to positional args
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
   <> help "Output raw JSON stream items" )
  argRaw <- switch
    ( long "raw"
   <> help "Output raw content JSON (skip templates)" )
  argDryRun <- switch
    ( long "dry-run" <> short 'n'
   <> help "Show JSON-RPC request without sending" )
  argSchema <- switch
    ( long "schema" <> short 's'
   <> help "Fetch raw schema JSON for path" )
  argGenerate <- switch
    ( long "generate-templates" <> short 'g'
   <> help "Generate mustache templates from schemas" )
  argParams <- optional $ T.pack <$> strOption
    ( long "params" <> short 'p' <> metavar "JSON"
   <> help "Method parameters as JSON object" )
  argRpc <- optional $ T.pack <$> strOption
    ( long "rpc" <> short 'r' <> metavar "JSON"
   <> help "Raw JSON-RPC request (bypass navigation)" )
  argPath <- many $ T.pack <$> argument str
    ( metavar "PATH... [--key value ...]"
   <> help "Path to plugin/method, with optional --key value params" )
  pure Args{..}
