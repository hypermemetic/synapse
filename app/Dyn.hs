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
import System.Environment (getArgs, withArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout, hPutStrLn, stderr)

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (PlexusConfig(..), plexusRpc)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema (PlexusSchema(..), PlexusSchemaEvent(..), ActivationInfo(..), extractSchemaEvent)
import Plexus.Schema.Cache
import Plexus.Dynamic

-- ============================================================================
-- Global Options (parsed before schema is available)
-- ============================================================================

data GlobalOpts = GlobalOpts
  { optRefresh :: Bool    -- ^ --refresh: force schema refetch
  , optHost    :: String  -- ^ --host: substrate host
  , optPort    :: Int     -- ^ --port: substrate port
  , optJson    :: Bool    -- ^ --json: raw JSON output
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
     <> help "Output raw JSON responses"
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

  -- Handle --help before loading schema
  when (null remaining || remaining == ["--help"] || remaining == ["-h"]) $ do
    schema <- loadSchema globalOpts
    case schema of
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
        exitFailure
      Right s -> do
        putStrLn "symbols-dyn - Dynamic CLI for Plexus"
        putStrLn ""
        putStrLn "Usage: symbols-dyn [GLOBAL_OPTIONS] COMMAND [ARGS...]"
        putStrLn ""
        putStrLn "Global options:"
        putStrLn "  -r, --refresh      Force refresh of cached schema"
        putStrLn "  -H, --host HOST    Substrate host (default: 127.0.0.1)"
        putStrLn "  -P, --port PORT    Substrate port (default: 4444)"
        putStrLn "  -j, --json         Output raw JSON responses"
        putStrLn ""
        putStrLn "Available commands:"
        mapM_ printActivation (schemaActivations s)
        exitFailure

  -- Load schema (from cache or fetch fresh)
  schemaResult <- loadSchema globalOpts
  schema <- case schemaResult of
    Left err -> do
      hPutStrLn stderr $ "Failed to load schema: " <> T.unpack err
      hPutStrLn stderr "Make sure the substrate is running, or use --refresh to refetch"
      exitFailure
    Right s -> pure s

  -- Parse remaining args with dynamic parser
  let dynamicInfo = info (buildDynamicParser schema <**> helper)
        ( fullDesc
       <> progDesc "Execute Plexus RPC methods"
        )

  invocation <- case execParserPure defaultPrefs dynamicInfo remaining of
    Success inv -> pure inv
    Failure err -> do
      let (msg, _) = renderFailure err "symbols-dyn"
      hPutStrLn stderr msg
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
      | x `elem` ["--refresh", "-r", "--json", "-j"] =
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

loadSchema :: GlobalOpts -> IO (Either Text PlexusSchema)
loadSchema opts = do
  config <- defaultCacheConfig
  loadSchemaWithCache (optRefresh opts) config (fetchSchemaFromSubstrate opts)

-- | Fetch schema fresh from substrate
fetchSchemaFromSubstrate :: GlobalOpts -> IO (Either Text PlexusSchema)
fetchSchemaFromSubstrate opts = do
  let config = defaultConfig
        { plexusHost = optHost opts
        , plexusPort = optPort opts
        }
  doFetch config `catch` \(e :: SomeException) ->
    pure $ Left $ T.pack $ "Connection error: " <> show e
  where
    doFetch config = do
      conn <- connect config
      mSchema <- S.head_ $ S.mapMaybe extractSchemaEvent $
        plexusRpc conn "plexus_schema" (toJSON ([] :: [Value]))
      disconnect conn
      case mSchema of
        Just (SchemaData s) -> pure $ Right s
        Just (SchemaError e) -> pure $ Left e
        Nothing -> pure $ Left "No schema received from substrate"

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

  -- Execute RPC call
  S.mapM_ (printResult opts) $
    plexusRpc conn invMethod invParams

  disconnect conn

-- | Print a stream item
printResult :: GlobalOpts -> PlexusStreamItem -> IO ()
printResult opts item
  | optJson opts = LBS.putStrLn $ encode item
  | otherwise = case item of
      StreamProgress _ msg _ -> do
        T.putStr msg
        putStr "\r"
        hFlush stdout
      StreamData _ contentType dat -> do
        T.putStrLn $ "=== " <> contentType <> " ==="
        LBS.putStrLn $ Aeson.encode dat
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
