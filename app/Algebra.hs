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
  , argParams  :: Maybe Text
  , argPath    :: [Text]
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
  -- No path: show CLI help + plexus schema
  if null argPath
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
      view <- navigate argPath
      case view of
        -- Landed on a plugin: show help
        ViewPlugin schema _ ->
          liftIO $ TIO.putStr $ renderSchema schema

        -- Landed on a method: invoke or show help
        ViewMethod method path
          | Just jsonStr <- argParams -> do
              -- Parse and invoke
              case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                Left err -> throwParse $ T.pack err
                Right params -> do
                  let namespacePath = init path  -- path without method name
                  let methodName' = last path
                  if argDryRun
                    then liftIO $ LBS.putStrLn $ encodeDryRun namespacePath methodName' params
                    else do
                      items <- invoke namespacePath methodName' params
                      liftIO $ mapM_ (printResult argJson) items

          | otherwise ->
              -- No params: show method help
              liftIO $ TIO.putStrLn $ renderMethodFull method

-- | Get CLI help text from optparse-applicative
cliHeader :: Text
cliHeader = T.pack $ fst $ renderFailure failure "synapse"
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
 <> header "synapse-algebra - Categorical CLI for Plexus"
 <> progDesc "Navigate and invoke Plexus methods using algebraic machinery"
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
  argParams <- optional $ T.pack <$> strOption
    ( long "params" <> short 'p' <> metavar "JSON"
   <> help "Method parameters as JSON object" )
  argPath <- many $ T.pack <$> argument str
    ( metavar "PATH..."
   <> help "Path to plugin/method" )
  pure Args{..}
