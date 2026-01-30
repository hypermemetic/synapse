{-# LANGUAGE OverloadedStrings #-}

-- | Command dispatcher for _self meta-commands
--
-- _self commands are client-side only and don't make RPC calls.
-- They operate on cached schemas and provide meta-functionality
-- like template generation, schema exploration, etc.
module Synapse.Self.Commands
  ( dispatch
  , showHelp
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Synapse.Monad
import Synapse.Backend.Discovery (getBackendAt)
import qualified Synapse.Self.Template as Template

-- | Dispatch to a specific _self subcommand
dispatch :: Text -> [Text] -> [(Text, Text)] -> SynapseM ()
dispatch "template" rest params = Template.handleTemplate rest params
dispatch "scan" rest _ = scanPorts rest
dispatch "--help" _ _ = showHelp
dispatch "-h" _ _ = showHelp
dispatch cmd _ _ =
  throwParse $ "Unknown _self command: " <> cmd <> "\n\n" <> helpText

-- | Scan ports for backends
-- Default range: 4440-4459 (covers 444x range)
scanPorts :: [Text] -> SynapseM ()
scanPorts _ = do
  host <- asks seHost
  liftIO $ TIO.putStrLn $ "Scanning " <> host <> " ports 4440-4459 for backends...\n"

  -- Scan ports in parallel
  let ports = [4440..4459]
  results <- liftIO $ mapConcurrently (probePort host) ports

  -- Display results
  let found = filter (\(_, mb) -> mb /= Nothing) (zip ports results)
  if null found
    then liftIO $ TIO.putStrLn "No backends found"
    else do
      liftIO $ TIO.putStrLn $ "Found " <> T.pack (show (length found)) <> " backend(s):\n"
      liftIO $ mapM_ printResult found
  where
    probePort :: Text -> Int -> IO (Maybe Text)
    probePort host port =
      getBackendAt host port `catch` \(_ :: SomeException) -> pure Nothing

    printResult :: (Int, Maybe Text) -> IO ()
    printResult (port, Just name) =
      TIO.putStrLn $ "  " <> T.pack (show port) <> "  " <> name
    printResult _ = pure ()

-- | Show help for _self commands
showHelp :: SynapseM ()
showHelp = liftIO $ TIO.putStr helpText

-- | Help text for _self commands
helpText :: Text
helpText = T.unlines
  [ "Meta-commands (local, no RPC):"
  , ""
  , "Available commands:"
  , ""
  , "  synapse _self scan"
  , "      Scan ports 4440-4459 for backends (calls _info on each)"
  , ""
  , "  synapse _self template"
  , "      Manage Mustache templates (CRUD operations)"
  , ""
  , "      Subcommands:"
  , "        list [pattern]      - List existing templates"
  , "        show <method>       - Display template content"
  , "        generate [pattern]  - Generate new templates from IR"
  , "        delete <pattern>    - Delete templates matching pattern"
  , "        reload              - Clear template cache"
  , ""
  , "      Pattern format: backend.namespace.method"
  , "      Examples: 'plexus.cone.*', 'plexus.*.create'"
  , ""
  , "Examples:"
  , "  synapse _self scan"
  , "  synapse _self template list"
  , "  synapse _self template show cone.chat"
  , "  synapse _self template generate 'plexus.cone.*'"
  , "  synapse _self template delete 'plexus.cone.test'"
  , ""
  , "Templates location: ~/.config/synapse/templates/"
  , ""
  , "Use 'synapse _self template' for detailed template help"
  , ""
  ]
