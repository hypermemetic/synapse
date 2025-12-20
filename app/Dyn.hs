{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | Dynamic CLI - discovers capabilities from the substrate at runtime
--
-- This CLI fetches the PlexusSchema from the substrate, caches it locally,
-- and dynamically builds subcommands from the available activations and methods.
module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.Aeson (Value(..), encode, toJSON, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import qualified Streaming.Prelude as S
import System.Environment (getArgs, withArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import qualified System.Directory
import System.IO (hFlush, stdout, hPutStrLn, stderr)

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (PlexusConfig(..), plexusRpc)
import Plexus.Types (PlexusStreamItem(..), GuidanceErrorType(..), GuidanceSuggestion(..))
import Plexus.Schema (PlexusSchema(..), PlexusSchemaEvent(..), ActivationInfo(..), EnrichedSchema(..), SchemaProperty(..), ActivationSchemaEvent(..), ActivationFullSchema(..), MethodSchemaInfo(..), FullSchemaEvent(..), PlexusHash(..), PlexusHashEvent(..), extractSchemaEvent, extractActivationSchemaEvent, extractFullSchemaEvent, extractHashEvent, MethodSchema(..), parseMethodSchemas)
import Plexus.Schema.Cache
import Plexus.Dynamic (CommandInvocation(..), buildDynamicParserWithSchemas)
import Plexus.Renderer (formatPrettyJson, RendererConfig)
import qualified Plexus.Renderer as Renderer
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Global Options (parsed before schema is available)
-- ============================================================================

data GlobalOpts = GlobalOpts
  { optRefresh   :: Bool    -- ^ Always refresh by default; --no-refresh: use cached schema
  , optHost      :: String  -- ^ --host: substrate host
  , optPort      :: Int     -- ^ --port: substrate port
  , optJson      :: Bool    -- ^ --json: raw JSON output (full stream item)
  , optRaw       :: Bool    -- ^ --raw: raw JSON data only (no headers)
  , optNoRender  :: Bool    -- ^ --no-render: skip template rendering
  , optSchema    :: Bool    -- ^ --schema: dump JSON schema for method
  , optTemplate  :: Bool    -- ^ --template: open template file for editing
  }
  deriving stock (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> flag True False
      ( long "no-refresh"
     <> help "Use cached schema instead of fetching fresh"
      )
  <*> strOption
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> value "127.0.0.1"
     <> showDefault
     <> help "Substrate host"
      )
  <*> option auto
      ( long "port"
     <> short 'P'
     <> metavar "PORT"
     <> value 4444
     <> showDefault
     <> help "Substrate port"
      )
  <*> switch
      ( long "json"
     <> short 'j'
     <> help "Output raw JSON responses (full stream item)"
      )
  <*> switch
      ( long "raw"
     <> help "Output raw JSON data only (no headers, no formatting)"
      )
  <*> switch
      ( long "no-render"
     <> help "Skip template rendering (use default JSON output)"
      )
  <*> switch
      ( long "schema"
     <> short 's'
     <> help "Dump JSON schema for method (usage: --schema NAMESPACE METHOD)"
      )
  <*> switch
      ( long "template"
     <> short 't'
     <> help "Open template file for editing (usage: --template NAMESPACE METHOD)"
      )

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- getArgs

  -- Parse global options, extracting remaining args for dynamic parser
  let (globalArgs, remaining) = splitArgs args

  -- Parse global options
  globalOpts <- case execParserPure defaultPrefs globalOptsInfo globalArgs of
    Success opts -> pure opts
    Failure err  -> do
      let (msg, _) = renderFailure err "symbols-dyn"
      hPutStrLn stderr msg
      exitFailure
    CompletionInvoked _ -> exitFailure

  -- Handle --schema mode: dump JSON schema for a method
  when (optSchema globalOpts) $ do
    case remaining of
      [ns] -> do
        -- Just namespace: show all methods in this activation
        mFullSchema <- fetchFullSchema globalOpts (T.pack ns)
        case mFullSchema of
          Nothing -> do
            hPutStrLn stderr $ "No schema found for namespace: " <> ns
            exitFailure
          Just fullSchema -> do
            T.putStrLn $ formatPrettyJson (toJSON fullSchema)
            exitSuccess
      [ns, method] -> do
        -- Load both base schema and full schema
        cacheResult <- loadSchema globalOpts
        case cacheResult of
          Left err -> do
            hPutStrLn stderr $ "Failed to load schema: " <> T.unpack err
            exitFailure
          Right cached -> do
            -- Method name format: just method (with dashes converted to underscores)
            let methodName = T.replace "-" "_" (T.pack method)
            -- Get full schema for this namespace
            let mFullSchema = Map.lookup (T.pack ns) (cachedFullSchemas cached)
            case mFullSchema of
              Nothing -> do
                hPutStrLn stderr $ "No schema found for namespace: " <> ns
                exitFailure
              Just fullSchema -> do
                -- Find the method in the full schema
                let methods = fullSchemaMethods fullSchema
                case find (\m -> methodInfoName m == methodName) methods of
                  Nothing -> do
                    hPutStrLn stderr $ "No method found: " <> T.unpack methodName
                    let availableMethods = map methodInfoName methods
                    hPutStrLn stderr $ "Available methods: " <> T.unpack (T.intercalate ", " availableMethods)
                    exitFailure
                  Just methodInfo -> do
                    T.putStrLn $ formatPrettyJson (toJSON methodInfo)
                    exitSuccess
      _ -> do
        hPutStrLn stderr "Usage: symbols-dyn --schema NAMESPACE [METHOD]"
        hPutStrLn stderr "Examples:"
        hPutStrLn stderr "  symbols-dyn --schema arbor           # Show all arbor methods"
        hPutStrLn stderr "  symbols-dyn --schema arbor tree-list # Show specific method"
        exitFailure

  -- Handle --template mode: open template file for editing
  when (optTemplate globalOpts) $ do
    case remaining of
      [ns, method] -> do
        let methodFile = T.replace "-" "_" (T.pack method)
        let templateDir = ".substrate/templates/" <> ns
        let templatePath = templateDir <> "/" <> T.unpack methodFile <> ".mustache"
        -- Create directory if needed
        createDirectoryIfMissing True templateDir
        -- Create default template if it doesn't exist
        exists <- doesFileExist templatePath
        when (not exists) $ do
          writeFile templatePath defaultTemplateContent
          putStrLn $ "Created new template: " <> templatePath
        -- Open in editor
        editor <- getEditor
        putStrLn $ "Opening " <> templatePath <> " in " <> editor
        callProcess editor [templatePath]
        exitSuccess
      _ -> do
        hPutStrLn stderr "Usage: symbols-dyn --template NAMESPACE METHOD"
        hPutStrLn stderr "Example: symbols-dyn --template arbor tree-list"
        exitFailure

  -- Handle info/about/version subcommand
  when (not (null remaining) && head remaining `elem` ["info", "about", "version"]) $ do
    handleInfoCommand globalOpts
    exitSuccess

  -- Handle cache subcommand
  when (not (null remaining) && head remaining == "cache") $ do
    handleCacheCommand globalOpts (tail remaining)
    exitSuccess

  -- Handle call subcommand: direct RPC invocation
  when (not (null remaining) && head remaining == "call") $ do
    handleCallCommand globalOpts (tail remaining)
    exitSuccess

  -- Handle --help before loading schema
  when (null remaining || remaining == ["--help"] || remaining == ["-h"]) $ do
    cacheResult <- loadSchema globalOpts
    case cacheResult of
      Left err -> do
        hPutStrLn stderr $ "Failed to load schema: " <> T.unpack err
        putStrLn ""
        putStrLn "Usage: symbols-dyn [GLOBAL_OPTIONS] COMMAND [ARGS...]"
        putStrLn ""
        putStrLn "Global options:"
        putStrLn "  --no-refresh       Use cached schema (default: always refresh)"
        putStrLn "  -H, --host HOST    Substrate host (default: 127.0.0.1)"
        putStrLn "  -P, --port PORT    Substrate port (default: 4444)"
        putStrLn "  -j, --json         Output raw JSON responses"
        putStrLn "  --no-render        Skip template rendering (use JSON output)"
        putStrLn "  -s, --schema       Dump JSON schema for a method"
        exitFailure
      Right cached -> do
        putStrLn "symbols-dyn - Dynamic CLI for Plexus"
        putStrLn ""
        putStrLn "Usage: symbols-dyn [GLOBAL_OPTIONS] COMMAND [ARGS...]"
        putStrLn ""
        putStrLn "Global options:"
        putStrLn "  --no-refresh       Use cached schema (default: always refresh)"
        putStrLn "  -H, --host HOST    Substrate host (default: 127.0.0.1)"
        putStrLn "  -P, --port PORT    Substrate port (default: 4444)"
        putStrLn "  -j, --json         Output raw JSON responses"
        putStrLn "  --no-render        Skip template rendering (use JSON output)"
        putStrLn "  -s, --schema       Dump JSON schema for a method"
        putStrLn ""
        putStrLn "Built-in commands:"
        putStrLn "  info               Show system info (aliases: about, version)"
        putStrLn "  cache              Manage schema cache (show, clear, status, refresh)"
        putStrLn "  call               Call any RPC method directly with JSON params"
        putStrLn ""
        putStrLn "Available activation commands:"
        mapM_ printActivation (schemaActivations (cachedSchema cached))
        exitFailure

  -- Load schema (from cache or fetch fresh)
  cacheResult <- loadSchema globalOpts
  cached <- case cacheResult of
    Left err -> do
      hPutStrLn stderr $ "Failed to load schema: " <> T.unpack err
      hPutStrLn stderr "Make sure the substrate is running, or use --refresh to refetch"
      exitFailure
    Right c -> pure c

  let schema = cachedSchema cached
  let fullSchemas = cachedFullSchemas cached

  -- Parse remaining args with dynamic parser (using full schemas for descriptions)
  let dynamicInfo = info (buildDynamicParserWithSchemas schema fullSchemas <**> helper)
        ( fullDesc
       <> progDesc "Execute Plexus RPC methods"
        )

  invocation <- case execParserPure defaultPrefs dynamicInfo remaining of
    Success inv -> pure inv
    Failure _ -> do
      -- On parse failure, show help for whatever level we're at
      case execParserPure defaultPrefs dynamicInfo (remaining ++ ["--help"]) of
        Failure helpErr -> do
          let (msg, _) = renderFailure helpErr "symbols-dyn"
          putStrLn msg
        _ -> pure ()
      exitFailure
    CompletionInvoked _ -> exitFailure

  -- Execute the RPC call (pass schema info for showing help on errors)
  executeCommand globalOpts invocation schema fullSchemas

-- | Split args into global and remaining
-- Global args are: --no-refresh, --host, -H, --port, -P, --json, -j, --raw, --no-render, --schema, -s, --template, -t
-- and their values
splitArgs :: [String] -> ([String], [String])
splitArgs = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
      | x `elem` ["--no-refresh", "--json", "-j", "--raw", "--no-render", "--schema", "-s", "--template", "-t"] =
          go (x:acc) xs
      | x `elem` ["--host", "-H", "--port", "-P"] =
          case xs of
            (v:rest) -> go (v:x:acc) rest
            [] -> (reverse (x:acc), [])
      | otherwise = (reverse acc, x:xs)

globalOptsInfo :: ParserInfo GlobalOpts
globalOptsInfo = info (globalOptsParser <**> helper)
  ( fullDesc
 <> progDesc "Dynamic CLI for Plexus"
  )

-- ============================================================================
-- Schema Loading
-- ============================================================================

loadSchema :: GlobalOpts -> IO (Either Text CachedSchema)
loadSchema opts = do
  config <- defaultCacheConfig
  loadSchemaWithCache
    (optRefresh opts)
    config
    (fetchHashFromSubstrate opts)
    (fetchSchemaFromSubstrate opts)
    (fetchFullSchema opts)

-- | Fetch plexus hash from substrate
fetchHashFromSubstrate :: GlobalOpts -> IO (Either Text Text)
fetchHashFromSubstrate opts = do
  let plexusCfg = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  doFetch plexusCfg `catch` \(e :: SomeException) ->
    pure $ Left $ T.pack $ "Connection error: " <> show e
  where
    doFetch cfg = do
      conn <- connect cfg
      mHash <- S.head_ $ S.mapMaybe extractHashEvent $
        plexusRpc conn "plexus_hash" (toJSON ([] :: [Value]))
      disconnect conn
      case mHash of
        Just (HashData h) -> pure $ Right (plexusHash h)
        Just (HashError e) -> pure $ Left e
        Nothing -> pure $ Left "No hash received from substrate"

-- | Fetch schema fresh from substrate
fetchSchemaFromSubstrate :: GlobalOpts -> IO (Either Text PlexusSchema)
fetchSchemaFromSubstrate opts = do
  let plexusCfg = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  doFetch plexusCfg `catch` \(e :: SomeException) ->
    pure $ Left $ T.pack $ "Connection error: " <> show e
  where
    doFetch cfg = do
      conn <- connect cfg
      mSchema <- S.head_ $ S.mapMaybe extractSchemaEvent $
        plexusRpc conn "plexus_schema" (toJSON ([] :: [Value]))
      disconnect conn
      case mSchema of
        Just (SchemaData s) -> pure $ Right s
        Just (SchemaError e) -> pure $ Left e
        Nothing -> pure $ Left "No schema received from substrate"

-- | Fetch enriched schema for a specific namespace
-- Gracefully returns Nothing if the endpoint doesn't exist
fetchFullSchema :: GlobalOpts -> Text -> IO (Maybe ActivationFullSchema)
fetchFullSchema opts namespace = do
  let plexusCfg = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  doFetch plexusCfg `catch` \(_ :: SomeException) -> pure Nothing
  where
    doFetch cfg = do
      conn <- connect cfg
      mSchema <- S.head_ $ S.mapMaybe extractFullSchemaEvent $
        plexusRpc conn "plexus_full_schema" (toJSON [namespace])
      disconnect conn
      case mSchema of
        Just (FullSchemaData s) -> pure $ Just s
        _ -> pure Nothing

-- ============================================================================
-- Command Execution
-- ============================================================================

executeCommand :: GlobalOpts -> CommandInvocation -> PlexusSchema -> Map.Map Text ActivationFullSchema -> IO ()
executeCommand opts CommandInvocation{..} schema fullSchemas = do
  let config = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  conn <- connect config

  -- Load renderer config
  rendererCfg <- Renderer.defaultConfig

  -- Parse namespace and method from invMethod (e.g., "arbor_tree_list" -> ("arbor", "tree_list"))
  let (namespace, method) = parseMethodName invMethod

  -- Create IORef for storing pending guidance
  guidanceRef <- newIORef Nothing

  -- Execute RPC call with rendering
  S.mapM_ (printResult opts rendererCfg namespace method guidanceRef schema fullSchemas) $
    plexusRpc conn invMethod invParams

  disconnect conn

-- | Parse method name into namespace and method
-- e.g., "arbor_tree_list" -> ("arbor", "tree_list")
parseMethodName :: Text -> (Text, Text)
parseMethodName methodName =
  case T.breakOn "_" methodName of
    (ns, rest) | not (T.null rest) -> (ns, T.drop 1 rest)
    _ -> (methodName, "")

-- | Print a stream item with template rendering
printResult :: GlobalOpts -> RendererConfig -> Text -> Text -> IORef (Maybe PlexusStreamItem) -> PlexusSchema -> Map.Map Text ActivationFullSchema -> PlexusStreamItem -> IO ()
printResult opts _ _ _ _ _ _ item
  | optJson opts = LBS.putStrLn $ encode item
printResult opts _ _ _ _ _ _ item
  | optRaw opts = case item of
      StreamData _ _ _ dat -> LBS.putStrLn $ encode dat
      StreamError _ _ err _ -> hPutStrLn stderr $ "Error: " <> T.unpack err
      _ -> pure ()
printResult opts rendererCfg namespace method guidanceRef schema fullSchemas item = case item of
  StreamProgress _ _ msg _ -> do
    T.putStr msg
    putStr "\r"
    hFlush stdout
  StreamData _ _ contentType dat -> do
    if optNoRender opts
      then do
        -- Skip template rendering, use default JSON output
        T.putStrLn $ "=== " <> contentType <> " ==="
        T.putStrLn $ formatPrettyJson dat
      else do
        -- Enrich data for better template rendering (e.g., merge tool_name into tool_input)
        let enrichedDat = enrichEventData dat
        -- Try to render with template
        result <- Renderer.render rendererCfg namespace method enrichedDat
        case result of
          Right rendered -> do
            -- Template found: print without header (for clean streaming output)
            T.putStr rendered
            hFlush stdout
            -- Add newline on chat completion to prevent shell % prompt
            case Aeson.decode (encode dat) of
              Just (Object o) | KM.lookup "type" o == Just (String "chat_complete") -> putStrLn ""
              Just (Object o) | KM.lookup "type" o == Just (String "complete") -> putStrLn ""
              _ -> pure ()
          Left _ -> do
            -- No template: fall back to pretty JSON with header
            T.putStrLn $ "=== " <> contentType <> " ==="
            T.putStrLn $ formatPrettyJson dat
  StreamGuidance{} -> do
    -- Store guidance for pairing with next error
    writeIORef guidanceRef (Just item)
  StreamError _ _ err _ -> do
    -- Check if we have pending guidance
    mbGuidance <- readIORef guidanceRef
    writeIORef guidanceRef Nothing

    case mbGuidance of
      Just guidance -> do
        -- Error with guidance - format with helpful suggestions
        hPutStrLn stderr $ "Error: " <> T.unpack err
        hPutStrLn stderr ""
        T.hPutStrLn stderr $ formatGuidance guidance
      Nothing -> do
        -- Check if this is a parameter error
        if isParamError err
          then do
            hPutStrLn stderr $ "Error: " <> T.unpack err
            hPutStrLn stderr ""
            -- Show the actual help for this command
            showCommandHelp namespace method schema fullSchemas
          else
            hPutStrLn stderr $ "Error: " <> T.unpack err
  StreamDone _ _ -> pure ()

-- | Show command-specific help when params are invalid
showCommandHelp :: Text -> Text -> PlexusSchema -> Map.Map Text ActivationFullSchema -> IO ()
showCommandHelp namespace method schema fullSchemas = do
  -- Build the parser for this specific command
  let dynamicInfo = info (buildDynamicParserWithSchemas schema fullSchemas <**> helper)
        ( fullDesc
       <> progDesc "Execute Plexus RPC methods"
        )
  -- Parse with --help to generate help output
  let args = [T.unpack namespace, T.unpack method, "--help"]
  case execParserPure defaultPrefs dynamicInfo args of
    Failure helpErr -> do
      let (msg, _) = renderFailure helpErr "symbols-dyn"
      putStrLn msg
    _ -> pure ()

-- | Check if an error is a parameter-related error
isParamError :: Text -> Bool
isParamError err =
  T.isInfixOf "-32602" err ||  -- JSON-RPC Invalid params error code
  T.isInfixOf "Invalid params" err ||
  T.isInfixOf "No more params" err ||
  T.isInfixOf "missing required" err ||
  T.isInfixOf "required parameter" err

-- | Format guidance into user-friendly suggestion text
formatGuidance :: PlexusStreamItem -> Text
formatGuidance StreamGuidance{..} =
  case itemErrorType of
    ActivationNotFound activation ->
      T.unlines
        [ "Activation '" <> activation <> "' not found"
        , ""
        , formatSuggestion itemSuggestion
        ]

    MethodNotFound activation method ->
      let availMethods = case itemAvailableMethods of
            Just methods -> "Available methods: " <> T.intercalate ", " methods
            Nothing -> "Run 'symbols-dyn " <> activation <> " --help' to see available methods"
      in T.unlines
        [ "Method '" <> method <> "' not found in activation '" <> activation <> "'"
        , availMethods
        , ""
        , formatSuggestion itemSuggestion
        ]

    InvalidParams method reason ->
      T.unlines
        [ "Invalid parameters for '" <> method <> "': " <> reason
        , ""
        , formatSuggestion itemSuggestion
        ]

formatGuidance _ = "Try: symbols-dyn --help"

-- | Format suggestion into actionable CLI command
formatSuggestion :: GuidanceSuggestion -> Text
formatSuggestion CallPlexusSchema = "Try: symbols-dyn --help"
formatSuggestion (CallActivationSchema namespace) = "Try: symbols-dyn " <> namespace <> " --help"
formatSuggestion (TryMethod method mbParams) =
  case mbParams of
    Just params -> "Try: symbols-dyn with method '" <> method <> "' and params: " <> T.pack (show params)
    Nothing -> "Try: symbols-dyn " <> method <> " --help"
formatSuggestion (CustomGuidance message) = "Suggestion: " <> message

-- ============================================================================
-- Info/About Command
-- ============================================================================

handleInfoCommand :: GlobalOpts -> IO ()
handleInfoCommand opts = do
  putStrLn ""
  putStrLn "    ███████╗██╗   ██╗███╗   ███╗██████╗  ██████╗ ██╗     ███████╗"
  putStrLn "    ██╔════╝╚██╗ ██╔╝████╗ ████║██╔══██╗██╔═══██╗██║     ██╔════╝"
  putStrLn "    ███████╗ ╚████╔╝ ██╔████╔██║██████╔╝██║   ██║██║     ███████╗"
  putStrLn "    ╚════██║  ╚██╔╝  ██║╚██╔╝██║██╔══██╗██║   ██║██║     ╚════██║"
  putStrLn "    ███████║   ██║   ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗███████║"
  putStrLn "    ╚══════╝   ╚═╝   ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝╚══════╝"
  putStrLn ""
  putStrLn "    Haskell frontend for Plexus - Typed APIs for LLM orchestration"
  putStrLn ""
  putStrLn "    Version:      0.1.0.0"
  putStrLn "    License:      MIT"
  putStrLn ""

  -- Try to load schema and show live backend info (always fetches fresh data)
  result <- loadSchema opts
  case result of
    Right cached -> do
      -- Show connection info with live backend hash
      putStrLn "    Connection:"
      putStrLn $ "      Endpoint:      ws://" <> optHost opts <> ":" <> show (optPort opts)
      putStrLn $ "      Backend Hash:  " <> T.unpack (cachedHash cached)
      putStrLn ""

      -- Show active activations (fresh from server)
      let activations = schemaActivations (cachedSchema cached)
      putStrLn $ "    Active Plexus Activations: " <> show (length activations)
      forM_ activations $ \act -> do
        let ns = activationNamespace act
        let desc = activationDescription act
        putStrLn $ "      • " <> T.unpack ns <> " - " <> T.unpack desc
      putStrLn ""
    Left err -> do
      -- Could not connect - show minimal info
      putStrLn "    ⚠  Could not connect to Plexus server"
      putStrLn $ "    Endpoint: ws://" <> optHost opts <> ":" <> show (optPort opts)
      putStrLn $ "    Error:    " <> T.unpack err
      putStrLn ""

  putStrLn "    Commands:"
  putStrLn "      symbols-dyn --help         Show full help"
  putStrLn "      symbols-dyn cache status   Check schema cache"
  putStrLn "      symbols-dyn <activation>   Run activation commands"
  putStrLn ""

-- ============================================================================
-- Cache Command
-- ============================================================================

handleCacheCommand :: GlobalOpts -> [String] -> IO ()
handleCacheCommand opts args = case args of
  ["show"] -> showCache opts
  ["clear"] -> clearCache
  ["status"] -> cacheStatus opts
  ["refresh"] -> refreshCache opts
  _ -> do
    hPutStrLn stderr "Usage: symbols-dyn cache COMMAND"
    hPutStrLn stderr ""
    hPutStrLn stderr "Commands:"
    hPutStrLn stderr "  show      Show cache contents (hash, schemas)"
    hPutStrLn stderr "  clear     Clear the cache"
    hPutStrLn stderr "  status    Check if cache is fresh (matches current hash)"
    hPutStrLn stderr "  refresh   Force refresh the cache"

showCache :: GlobalOpts -> IO ()
showCache _ = do
  config <- defaultCacheConfig
  mCached <- loadCache (cachePath config)
  case mCached of
    Nothing -> putStrLn "No cache found"
    Just cached -> do
      putStrLn "Cache contents:"
      putStrLn $ "  Hash: " <> T.unpack (cachedHash cached)
      putStrLn $ "  Activations: " <> show (length (schemaActivations (cachedSchema cached)))
      putStrLn $ "  Full schemas: " <> show (Map.size (cachedFullSchemas cached))
      putStrLn ""
      putStrLn "Activations:"
      mapM_ (\a -> putStrLn $ "  - " <> T.unpack (activationNamespace a) <>
                              " (" <> show (length (activationMethods a)) <> " methods)")
            (schemaActivations (cachedSchema cached))

clearCache :: IO ()
clearCache = do
  config <- defaultCacheConfig
  exists <- doesFileExist (cachePath config)
  if exists
    then do
      removeFile (cachePath config)
      putStrLn $ "Cache cleared: " <> cachePath config
    else
      putStrLn "No cache to clear"

cacheStatus :: GlobalOpts -> IO ()
cacheStatus opts = do
  config <- defaultCacheConfig
  mCached <- loadCache (cachePath config)
  case mCached of
    Nothing -> putStrLn "No cache found"
    Just cached -> do
      hashResult <- fetchHashFromSubstrate opts
      case hashResult of
        Left err -> do
          hPutStrLn stderr $ "Failed to fetch current hash: " <> T.unpack err
          putStrLn $ "Cached hash: " <> T.unpack (cachedHash cached)
          putStrLn "Status: Unknown (can't reach substrate)"
        Right currentHash -> do
          putStrLn $ "Cached hash:  " <> T.unpack (cachedHash cached)
          putStrLn $ "Current hash: " <> T.unpack currentHash
          if isFresh currentHash cached
            then putStrLn "Status: Fresh ✓"
            else putStrLn "Status: Stale (hash mismatch)"

refreshCache :: GlobalOpts -> IO ()
refreshCache opts = do
  putStrLn "Refreshing cache..."
  result <- loadSchema (opts { optRefresh = True })
  case result of
    Left err -> hPutStrLn stderr $ "Failed to refresh: " <> T.unpack err
    Right cached -> do
      putStrLn $ "Cache refreshed successfully"
      putStrLn $ "  Hash: " <> T.unpack (cachedHash cached)
      putStrLn $ "  Activations: " <> show (length (schemaActivations (cachedSchema cached)))

-- ============================================================================
-- Call Command (Direct RPC invocation)
-- ============================================================================

handleCallCommand :: GlobalOpts -> [String] -> IO ()
handleCallCommand opts args = case args of
  [method, paramsJson] -> do
    -- Parse JSON params
    case eitherDecode (LBS.pack paramsJson) of
      Left err -> do
        hPutStrLn stderr $ "Invalid JSON params: " <> err
        hPutStrLn stderr "Example: symbols-dyn call plexus_schema '[]'"
        exitFailure
      Right params -> do
        -- Connect and make RPC call
        let config = defaultConfig
              { plexusHost = optHost opts
              , plexusPort = optPort opts
              }
        conn <- connect config

        -- Load renderer config
        rendererCfg <- Renderer.defaultConfig

        -- Create IORef for guidance tracking
        guidanceRef <- newIORef Nothing

        -- Stream results (no schema for generic call command)
        S.mapM_ (printResult opts rendererCfg "" (T.pack method) guidanceRef (PlexusSchema [] 0) Map.empty) $
          plexusRpc conn (T.pack method) params

        disconnect conn
  _ -> do
    hPutStrLn stderr "Usage: symbols-dyn call METHOD PARAMS_JSON"
    hPutStrLn stderr ""
    hPutStrLn stderr "Examples:"
    hPutStrLn stderr "  symbols-dyn call plexus_schema '[]'"
    hPutStrLn stderr "  symbols-dyn call plexus_activation_schema '[\"cone\"]'"
    hPutStrLn stderr "  symbols-dyn call plexus_hash '[]'"
    hPutStrLn stderr "  symbols-dyn call arbor_tree_list '{}'"
    hPutStrLn stderr "  symbols-dyn call cone_chat '{\"identifier\":{\"by_name\":{\"name\":\"test\"}},\"prompt\":\"hi\"}'"

-- ============================================================================
-- Help Formatting
-- ============================================================================

printActivation :: ActivationInfo -> IO ()
printActivation act = do
  let ns = activationNamespace act
  let desc = activationDescription act
  putStrLn $ "  " <> T.unpack ns <> replicate (14 - T.length ns) ' ' <> T.unpack desc

-- | Enrich event data for better template rendering
-- For tool_use events, merges tool_name into tool_input for easier access in templates
enrichEventData :: Value -> Value
enrichEventData (Object o) =
  case (KM.lookup "tool_name" o, KM.lookup "tool_input" o) of
    (Just (String toolName), Just (Object toolInput)) ->
      -- Add tool_name to tool_input for easier template access
      let enrichedInput = KM.insert "tool_name" (String toolName) toolInput
      in Object $ KM.insert "tool_input" (Object enrichedInput) o
    _ -> Object o
enrichEventData v = v

-- | when helper (not in prelude for older GHC)
when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = pure ()

-- | find helper
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- | findIndex helper
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = go 0 xs
  where
    go _ [] = Nothing
    go i (y:ys) = if p y then Just i else go (i+1) ys

-- ============================================================================
-- Schema Helpers
-- ============================================================================

-- | Find a method variant in the enriched schema by name
-- Looks through oneOf variants and matches against the method property
findMethodVariant :: Text -> EnrichedSchema -> Maybe EnrichedSchema
findMethodVariant targetMethod schema = case schemaOneOf schema of
  Just variants -> find (matchesMethod targetMethod) variants
  Nothing -> Nothing
  where
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

-- | Check if a schema variant matches the target method name
matchesMethod :: Text -> EnrichedSchema -> Bool
matchesMethod targetMethod variant = case schemaProperties variant of
  Just props -> case Map.lookup "method" props of
    Just methodProp -> case propType methodProp of
      -- Check if the const value matches (if present)
      Just (String t) | t == targetMethod -> True
      _ -> case propEnum methodProp of
        Just vals -> any (== String targetMethod) vals
        Nothing -> False
    Nothing -> False
  Nothing -> False

-- | List all method names from an enriched schema
listMethodNames :: EnrichedSchema -> [Text]
listMethodNames schema = case schemaOneOf schema of
  Just variants -> concatMap extractMethodFromVariant variants
  Nothing -> []
  where
    extractMethodFromVariant v = case schemaProperties v of
      Just props -> case Map.lookup "method" props of
        Just methodProp -> case propType methodProp of
          Just (String t) -> [t]
          _ -> case propEnum methodProp of
            Just vals -> [t | String t <- vals]
            Nothing -> []
        Nothing -> []
      Nothing -> []

-- ============================================================================
-- Template Helpers
-- ============================================================================

-- | Get editor from environment or default
getEditor :: IO String
getEditor = do
  mEditor <- lookupEnv "EDITOR"
  pure $ case mEditor of
    Just e -> e
    Nothing -> "vi"

-- | Default template content for new templates
defaultTemplateContent :: String
defaultTemplateContent = unlines
  [ "{{! Template for this method }}"
  , "{{! Use {{variable}} for substitution }}"
  , "{{! Use {{#array}}...{{/array}} for iteration }}"
  , "{{! Use {{.}} for current item in iteration }}"
  , ""
  , "{{type}}"
  ]
