-- | Completion functions for shell tab-completion
module Synapse.Algebra.Complete
  ( completions
  , completeMethods
  , completeChildren
  ) where

import Data.Text (Text)
import Synapse.Schema.Types

-- | Get all completions (methods + children)
completions :: PluginSchema -> [Text]
completions PluginSchema{..} = concat
  [ map methodName psMethods
  , maybe [] (map csNamespace) psChildren
  ]

-- | Get method completions only
completeMethods :: PluginSchema -> [Text]
completeMethods = map methodName . psMethods

-- | Get child namespace completions only
completeChildren :: PluginSchema -> [Text]
completeChildren = maybe [] (map csNamespace) . psChildren
