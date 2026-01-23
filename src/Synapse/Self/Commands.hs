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

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Synapse.Monad
import qualified Synapse.Self.Template as Template

-- | Dispatch to a specific _self subcommand
dispatch :: Text -> [Text] -> [(Text, Text)] -> SynapseM ()
dispatch "template" rest params = Template.handleTemplate rest params
dispatch "--help" _ _ = showHelp
dispatch "-h" _ _ = showHelp
dispatch cmd _ _ =
  throwParse $ "Unknown _self command: " <> cmd <> "\n\n" <> helpText

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
  , "  synapse _self template <pattern>"
  , "      Generate example CLI invocations from schemas"
  , ""
  , "      Pattern examples:"
  , "        plexus.cone.create      # Exact match"
  , "        plexus.cone.*           # All cone methods"
  , "        plexus.*.create         # All create methods across activations"
  , "        plexus.*.*              # All plexus methods (default)"
  , ""
  , "      Options:"
  , "        --limit N                           # Show first N results (default: 10)"
  , "        --limit.lower L --limit.upper U     # Show results L through U"
  , ""
  , "Examples:"
  , "  synapse _self template 'plexus.cone.*'"
  , "  synapse _self template 'plexus.*.create' --limit 5"
  , "  synapse _self template 'plexus.arbor.*' --limit.lower 10 --limit.upper 15"
  , ""
  , "Note: Patterns match backend.activation.method format"
  , "      Use quotes to prevent shell expansion of wildcards"
  , ""
  ]
