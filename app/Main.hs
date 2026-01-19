{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}

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
import Control.Monad.Reader (asks)
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


import Synapse.Schema.Types
import Synapse.Monad (SynapseM, SynapseEnv(..), SynapseError(..), initEnv, runSynapseM, throwNav, throwTransport, throwParse)
import Synapse.Algebra.Navigate
import Synapse.Algebra.Render (renderSchema)
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.CLI.Parse (parseParams)
import qualified Synapse.CLI.Parse as Parse
import Synapse.IR.Types (IR, irMethods, MethodDef)
import qualified Data.Map.Strict as Map
import qualified Synapse.CLI.Template as TemplateIR
import Synapse.Transport
import Synapse.CLI.Transform (mkTransformEnv, transformParams, defaultTransformers, injectBooleanDefaults)
import Synapse.IR.Builder (buildIR)
import Synapse.Renderer (RendererConfig, defaultRendererConfig, renderItem, prettyValue, withMethodPath)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

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
  , argGenerate  :: Bool          -- ^ Generate templates from IR
  , argEmitIR    :: Bool          -- ^ Emit IR for code generation
  , argForce     :: Bool          -- ^ Force overwrite modified templates (unused)
  , argParams    :: Maybe Text    -- ^ JSON params via -p
  , argRpc       :: Maybe Text    -- ^ Raw JSON-RPC passthrough
  , argBackend   :: Maybe Text    -- ^ Backend name (first positional arg)
  , argPath      :: [Text]        -- ^ Path segments and --key value params
  }
  deriving Show

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- execParser argsInfo
  case argBackend args of
    Nothing -> TIO.putStr cliHeader  -- No backend specified, show help
    Just backend
      -- Handle --help/-h as backend (forwardOptions treats flags as positional args)
      | backend `elem` ["--help", "-h"] -> TIO.putStr cliHeader
      | otherwise -> run backend args

-- | Run a Hub command with specified backend
run :: Text -> Args -> IO ()
run backend args = do
  env <- initEnv (argHost args) (argPort args) backend
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
      let (pathSegs, rawParams, helpRequested) = parsePathAndParams argPath

      -- Apply parameter transformations (path expansion, env vars)
      transformEnv <- liftIO mkTransformEnv
      inlineParams <- liftIO $ transformParams transformEnv defaultTransformers rawParams

      -- Mode 2: Schema request
      if argSchema
        then do
          -- Try to navigate and determine if last segment is a method
          schemaResult <- fetchSchemaForPath pathSegs
          case schemaResult of
            Left err -> throwNav $ FetchError err pathSegs
            Right val -> liftIO $ LBS.putStrLn $ encode val

        -- Mode 3: Generate templates (IR-driven approach)
        else if argGenerate
        then do
          homeDir <- liftIO getHomeDirectory
          let baseDir = homeDir </> ".config" </> "synapse" </> "templates"
              writeAndLog gt = do
                writeGeneratedTemplateIR baseDir gt
                TIO.putStrLn $ "  " <> T.pack (TemplateIR.gtPath gt)
          liftIO $ TIO.putStrLn $ "Generating templates in " <> T.pack baseDir <> "..."
          count <- TemplateIR.generateAllTemplatesWithCallback writeAndLog pathSegs
          liftIO $ TIO.putStrLn $ "Generated " <> T.pack (show count) <> " templates"

        -- Mode 4: Emit IR for code generation
        else if argEmitIR
        then do
          ir <- buildIR pathSegs
          liftIO $ LBS.putStrLn $ encode ir

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
                  -- Build IR for this method's namespace
                  ir <- buildIR (init path)
                  let fullPath = T.intercalate "." path

                  -- If --help was explicitly requested, show help and exit
                  if helpRequested
                    then do
                      case Map.lookup fullPath (irMethods ir) of
                        Just methodDef ->
                          liftIO $ TIO.putStr $ renderMethodHelp ir methodDef
                        Nothing ->
                          liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                    else do
                      -- Build params: use IR-driven parsing for inline params
                      let schemaDefaults = extractSchemaDefaults method
                      userParams <- case argParams of
                        -- -p JSON: parse as raw JSON (bypass IR parsing)
                        Just jsonStr ->
                          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                            Left err -> throwParse $ T.pack err
                            Right p -> pure p
                        Nothing
                          | not (null inlineParams) ->
                              -- Use IR-driven parsing for inline params
                              case Map.lookup fullPath (irMethods ir) of
                                Just methodDef -> do
                                  -- Inject boolean defaults for flags without values
                                  let paramsWithBools = injectBooleanDefaults ir methodDef inlineParams
                                  case parseParams ir methodDef paramsWithBools of
                                    Right p -> pure p
                                    Left errs -> do
                                      liftIO $ mapM_ (hPutStrLn stderr . renderParseError) errs
                                      throwParse "Parameter parsing failed"
                                Nothing ->
                                  -- Fallback to flat object if method not in IR
                                  pure $ buildParamsObject inlineParams
                          | otherwise -> pure $ object []
                      -- Merge: user params override schema defaults
                      let params = mergeParams schemaDefaults userParams

                      if argDryRun
                        then do
                          backend <- asks seBackend
                          liftIO $ LBS.putStrLn $ encodeDryRun backend (init path) (last path) params
                        -- Show help when: method has required params AND user provided no params
                        else if hasRequiredParams method && userParams == object [] && null inlineParams
                          then do
                            -- Render help from IR
                            case Map.lookup fullPath (irMethods ir) of
                              Just methodDef ->
                                liftIO $ TIO.putStr $ renderMethodHelp ir methodDef
                              Nothing ->
                                -- Fallback: method not in IR, use basic info
                                liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                          else invokeMethod path params
  where
    invokeMethod path params = do
      let namespacePath = init path  -- path without method name
      let methodName' = last path
      -- Set method path hint for template resolution
      let rendererCfg' = withMethodPath rendererCfg path
      invokeStreaming namespacePath methodName' params (printResult argJson argRaw rendererCfg')

    -- Extract default values from JSON Schema properties
    -- Schema format: {"properties": {"key": {"default": value, ...}, ...}, ...}
    extractSchemaDefaults :: MethodSchema -> Value
    extractSchemaDefaults m = case methodParams m of
      Nothing -> object []
      Just (Object o) -> case KM.lookup "properties" o of
        Just (Object props) -> object
          [ (k, defaultVal)
          | (k, propSchema) <- KM.toList props
          , Object propObj <- [propSchema]
          , Just defaultVal <- [KM.lookup "default" propObj]
          ]
        _ -> object []
      Just _ -> object []

    -- Merge two JSON objects: right takes precedence over left
    mergeParams :: Value -> Value -> Value
    mergeParams (Object defaults) (Object user) =
      Object (KM.union user defaults)  -- union prefers first arg on conflict
    mergeParams _ user = user  -- if defaults aren't an object, just use user params

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
    invokeRawRpc :: Value -> SynapseM [HubStreamItem]
    invokeRawRpc rpcReq = do
      case rpcReq of
        Object o -> case (KM.lookup "method" o, KM.lookup "params" o) of
          (Just (String method), Just params) -> invokeRaw method params
          (Just (String method), Nothing) -> invokeRaw method (object [])
          _ -> throwParse "JSON-RPC must have 'method' field"
        _ -> throwParse "JSON-RPC must be an object"

-- | Parse path segments and --key value params
-- Returns (path segments, [(key, value)] params, help requested)
-- Example: ["echo", "once", "--message", "hello", "--count", "3"]
--       -> (["echo", "once"], [("message", "hello"), ("count", "3")], False)
-- Example: ["echo", "once", "--help"]
--       -> (["echo", "once"], [], True)
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)], Bool)
parsePathAndParams = go [] [] False
  where
    go path params helpReq [] = (reverse path, reverse params, helpReq)
    go path params helpReq (x:xs)
      -- --help flag: mark help requested, don't add as param
      | x == "--help" || x == "-h" =
          go path params True xs
      -- --key value pair (value must not start with --)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs
      , not (T.isPrefixOf "--" val) =
          go path ((key, val) : params) helpReq rest
      -- --key with no value or next arg is another flag (boolean flag)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key) =
          go path ((key, "") : params) helpReq xs
      -- Regular path segment - split on dots to support plexus.cone.chat syntax
      | otherwise =
          let segments = filter (not . T.null) $ T.splitOn "." x
          in go (reverse segments ++ path) params helpReq xs

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

-- | Render a parse error for display
renderParseError :: Parse.ParseError -> String
renderParseError = \case
  Parse.UnknownParam name ->
    "Unknown parameter: --" <> T.unpack name
  Parse.MissingRequired name ->
    "Missing required parameter: --" <> T.unpack name
  Parse.InvalidValue name reason ->
    "Invalid value for --" <> T.unpack name <> ": " <> T.unpack reason
  Parse.AmbiguousVariant name variants ->
    "Ambiguous variant for --" <> T.unpack name <> ": could be one of " <> show (map T.unpack variants)
  Parse.MissingDiscriminator param field ->
    "Missing discriminator for --" <> T.unpack param <> ": need --" <> T.unpack param <> "." <> T.unpack field
  Parse.UnknownVariant param value valid ->
    "Unknown variant '" <> T.unpack value <> "' for --" <> T.unpack param
    <> ". Valid variants: " <> T.unpack (T.intercalate ", " valid)
  Parse.TypeNotFound name ->
    "Type not found in IR: " <> T.unpack name

-- | Write a generated template to disk (no logging)
writeGeneratedTemplateIR :: FilePath -> TemplateIR.GeneratedTemplate -> IO ()
writeGeneratedTemplateIR baseDir gt = do
  let fullPath = baseDir </> TemplateIR.gtPath gt
  createDirectoryIfMissing True (baseDir </> T.unpack (TemplateIR.gtNamespace gt))
  TIO.writeFile fullPath (TemplateIR.gtTemplate gt)

-- | Encode a dry-run request
encodeDryRun :: Text -> [Text] -> Text -> Value -> LBS.ByteString
encodeDryRun backend namespacePath method params =
  let fullPath = if null namespacePath then [backend] else namespacePath
      dotPath = T.intercalate "." (fullPath ++ [method])
  in encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= (backend <> ".call")
    , "params" .= object
        [ "method" .= dotPath
        , "params" .= params
        ]
    ]

-- | Print a stream result
-- argJson: output raw JSON stream items
-- argRaw: skip template rendering, just output content JSON
-- otherwise: try template rendering, fall back to content JSON
printResult :: Bool -> Bool -> RendererConfig -> HubStreamItem -> IO ()
printResult True _ _ item = LBS.putStrLn $ encode item
printResult _ True _ item = case item of
  -- Raw mode: just output the content
  HubData _ _ _ dat -> do
    LBS.putStrLn $ encode dat
    hFlush stdout
  HubProgress _ _ msg _ -> do
    TIO.putStr msg
    TIO.putStr "\r"
    hFlush stdout
  HubError _ _ err _ ->
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
      HubData _ _ _ dat -> do
        TIO.putStrLn $ prettyValue dat
        hFlush stdout
      HubProgress _ _ msg _ -> do
        TIO.putStr msg
        TIO.putStr "\r"
        hFlush stdout
      HubError _ _ err _ ->
        hPutStrLn stderr $ "Error: " <> T.unpack err
      _ -> pure ()

-- ============================================================================
-- Argument Parsing
-- ============================================================================

argsInfo :: ParserInfo Args
argsInfo = info argsParser
  ( fullDesc
 <> header "synapse - Algebraic CLI for Hub"
 <> progDesc "Navigate and invoke methods via coalgebraic schema traversal"
 <> forwardOptions  -- Pass unrecognized --flags to positional args
  )
  -- Note: We don't use `helper` here because we want --help to be forwarded
  -- to argPath so subcommand help works (e.g., `synapse plexus solar --help`)

argsParser :: Parser Args
argsParser = do
  argHost <- T.pack <$> strOption
    ( long "host" <> short 'H' <> metavar "HOST"
   <> value "127.0.0.1" <> showDefault
   <> help "Hub server host" )
  argPort <- option auto
    ( long "port" <> short 'P' <> metavar "PORT"
   <> value 4444 <> showDefault
   <> help "Hub server port" )
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
   <> help "Generate mustache templates from IR" )
  argEmitIR <- switch
    ( long "emit-ir" <> short 'i'
   <> help "Emit IR for code generation (JSON)" )
  let argForce = False  -- Not yet implemented
  argParams <- optional $ T.pack <$> strOption
    ( long "params" <> short 'p' <> metavar "JSON"
   <> help "Method parameters as JSON object" )
  argRpc <- optional $ T.pack <$> strOption
    ( long "rpc" <> short 'r' <> metavar "JSON"
   <> help "Raw JSON-RPC request (bypass navigation)" )
  argBackend <- optional $ T.pack <$> argument str
    ( metavar "BACKEND"
   <> help "Backend name (e.g., plexus)" )
  argPath <- many $ T.pack <$> argument str
    ( metavar "[PATH...] [--key value ...]"
   <> help "Path to plugin/method, with optional --key value params" )
  pure Args{..}
