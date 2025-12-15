{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | Dynamic CLI - discovers capabilities from the substrate at runtime
--
-- This CLI fetches the PlexusSchema from the substrate, caches it locally,
-- and dynamically builds subcommands from the available activations and methods.
module Main where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import qualified Streaming.Prelude as S
import System.Environment (getArgs, withArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (hFlush, stdout, hPutStrLn, stderr)

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (PlexusConfig(..), plexusRpc)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema (PlexusSchema(..), PlexusSchemaEvent(..), ActivationInfo(..), EnrichedSchema(..), SchemaProperty(..), ActivationSchemaEvent(..), extractSchemaEvent, extractActivationSchemaEvent, MethodSchema(..), parseMethodSchemas)
import Plexus.Schema.Cache
import Plexus.Dynamic (CommandInvocation(..), buildDynamicParserWithSchemas)
import Plexus.Renderer (formatPrettyJson, RendererConfig)
import qualified Plexus.Renderer as Renderer
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Global Options (parsed before schema is available)
-- ============================================================================

data GlobalOpts = GlobalOpts
  { optRefresh   :: Bool    -- ^ --refresh: force schema refetch
  , optHost      :: String  -- ^ --host: substrate host
  , optPort      :: Int     -- ^ --port: substrate port
  , optJson      :: Bool    -- ^ --json: raw JSON output (full stream item)
  , optRaw       :: Bool    -- ^ --raw: raw JSON data only (no headers)
  , optSchema    :: Bool    -- ^ --schema: dump JSON schema for method
  , optTemplate  :: Bool    -- ^ --template: open template file for editing
  }
  deriving stock (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
      ( long "refresh"
     <> short 'r'
     <> help "Force refresh of cached schema"
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
        mEnriched <- fetchEnrichedSchema globalOpts (T.pack ns)
        case mEnriched of
          Nothing -> do
            hPutStrLn stderr $ "No schema found for namespace: " <> ns
            exitFailure
          Just enriched -> do
            T.putStrLn $ formatPrettyJson (toJSON enriched)
            exitSuccess
      [ns, method] -> do
        -- Load both base schema (for method list) and enriched schema
        cacheResult <- loadSchema globalOpts
        case cacheResult of
          Left err -> do
            hPutStrLn stderr $ "Failed to load schema: " <> T.unpack err
            exitFailure
          Right cached -> do
            -- Find the activation in base schema to get method list
            let schema = cachedSchema cached
            let mActivation = find (\a -> activationNamespace a == T.pack ns)
                                   (schemaActivations schema)
            case mActivation of
              Nothing -> do
                hPutStrLn stderr $ "No activation found for namespace: " <> ns
                exitFailure
              Just activation -> do
                -- Method name format: just method (with dashes converted to underscores)
                let methodName = T.replace "-" "_" (T.pack method)
                let methods = activationMethods activation
                -- Find method index
                case findIndex (== methodName) methods of
                  Nothing -> do
                    hPutStrLn stderr $ "No method found: " <> T.unpack methodName
                    hPutStrLn stderr $ "Available methods: " <> T.unpack (T.intercalate ", " methods)
                    exitFailure
                  Just idx -> do
                    -- Get enriched schema and index into oneOf
                    let mEnriched = Map.lookup (T.pack ns) (cachedEnriched cached)
                    case mEnriched >>= schemaOneOf of
                      Nothing -> do
                        hPutStrLn stderr $ "No enriched schema for: " <> ns
                        exitFailure
                      Just variants | idx < length variants -> do
                        T.putStrLn $ formatPrettyJson (toJSON (variants !! idx))
                        exitSuccess
                      Just _ -> do
                        hPutStrLn stderr $ "Method index out of range: " <> show idx
                        exitFailure
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
        putStrLn "  -r, --refresh      Force refresh of cached schema"
        putStrLn "  -H, --host HOST    Substrate host (default: 127.0.0.1)"
        putStrLn "  -P, --port PORT    Substrate port (default: 4444)"
        putStrLn "  -j, --json         Output raw JSON responses"
        putStrLn "  -s, --schema       Dump JSON schema for a method"
        exitFailure
      Right cached -> do
        putStrLn "symbols-dyn - Dynamic CLI for Plexus"
        putStrLn ""
        putStrLn "Usage: symbols-dyn [GLOBAL_OPTIONS] COMMAND [ARGS...]"
        putStrLn ""
        putStrLn "Global options:"
        putStrLn "  -r, --refresh      Force refresh of cached schema"
        putStrLn "  -H, --host HOST    Substrate host (default: 127.0.0.1)"
        putStrLn "  -P, --port PORT    Substrate port (default: 4444)"
        putStrLn "  -j, --json         Output raw JSON responses"
        putStrLn "  -s, --schema       Dump JSON schema for a method"
        putStrLn ""
        putStrLn "Available commands:"
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
  let enriched = cachedEnriched cached

  -- Parse remaining args with dynamic parser (using enriched schemas for typed flags)
  let dynamicInfo = info (buildDynamicParserWithSchemas schema enriched <**> helper)
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

  -- Execute the RPC call
  executeCommand globalOpts invocation

-- | Split args into global and remaining
-- Global args are: --refresh, -r, --host, -H, --port, -P, --json, -j
-- and their values
splitArgs :: [String] -> ([String], [String])
splitArgs = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
      | x `elem` ["--refresh", "-r", "--json", "-j", "--raw", "--schema", "-s", "--template", "-t"] =
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
    (fetchSchemaFromSubstrate opts)
    (fetchEnrichedSchema opts)

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
fetchEnrichedSchema :: GlobalOpts -> Text -> IO (Maybe EnrichedSchema)
fetchEnrichedSchema opts namespace = do
  let plexusCfg = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  doFetch plexusCfg `catch` \(_ :: SomeException) -> pure Nothing
  where
    doFetch cfg = do
      conn <- connect cfg
      mSchema <- S.head_ $ S.mapMaybe extractActivationSchemaEvent $
        plexusRpc conn "plexus_activation_schema" (toJSON [namespace])
      disconnect conn
      case mSchema of
        Just (ActivationSchemaData s) -> pure $ Just s
        _ -> pure Nothing

-- ============================================================================
-- Command Execution
-- ============================================================================

executeCommand :: GlobalOpts -> CommandInvocation -> IO ()
executeCommand opts CommandInvocation{..} = do
  let config = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  conn <- connect config

  -- Load renderer config
  rendererCfg <- Renderer.defaultConfig

  -- Parse namespace and method from invMethod (e.g., "arbor_tree_list" -> ("arbor", "tree_list"))
  let (namespace, method) = parseMethodName invMethod

  -- Execute RPC call with rendering
  S.mapM_ (printResult opts rendererCfg namespace method) $
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
printResult :: GlobalOpts -> RendererConfig -> Text -> Text -> PlexusStreamItem -> IO ()
printResult opts _ _ _ item
  | optJson opts = LBS.putStrLn $ encode item
printResult opts _ _ _ item
  | optRaw opts = case item of
      StreamData _ _ dat -> LBS.putStrLn $ encode dat
      StreamError _ err _ -> hPutStrLn stderr $ "Error: " <> T.unpack err
      _ -> pure ()
printResult _ rendererCfg namespace method item = case item of
  StreamProgress _ msg _ -> do
    T.putStr msg
    putStr "\r"
    hFlush stdout
  StreamData _ contentType dat -> do
    -- Try to render with template
    result <- Renderer.render rendererCfg namespace method dat
    case result of
      Right rendered -> do
        T.putStrLn $ "=== " <> contentType <> " ==="
        T.putStrLn rendered
      Left _ -> do
        -- Fall back to pretty JSON
        T.putStrLn $ "=== " <> contentType <> " ==="
        T.putStrLn $ formatPrettyJson dat
  StreamError _ err _ -> do
    hPutStrLn stderr $ "Error: " <> T.unpack err
  StreamDone _ -> pure ()

-- ============================================================================
-- Help Formatting
-- ============================================================================

printActivation :: ActivationInfo -> IO ()
printActivation act = do
  let ns = activationNamespace act
  let desc = activationDescription act
  putStrLn $ "  " <> T.unpack ns <> replicate (14 - T.length ns) ' ' <> T.unpack desc

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
