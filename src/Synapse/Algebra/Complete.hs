-- | Completion algebra (catamorphism)
--
-- = The Algebra
--
-- @
-- completeAlg :: PluginSchemaF [Text] -> [Text]
-- @
--
-- Collects names at each level for shell tab-completion.
-- For shallow schemas, we only see one level at a time.
module Synapse.Algebra.Complete
  ( -- * Completion Algebras
    CompleteAlg
  , completeAlg
  , completeMethodsAlg
  , completeChildrenAlg

    -- * Completion Functions
  , completions
  , completeMethods
  , completeChildren
  ) where

import Data.Text (Text)

import Synapse.Schema.Types
import Synapse.Schema.Base

-- | The completion algebra type
--
-- For fully recursive schemas:
-- @
-- completeAlg :: PluginSchemaF [Text] -> [Text]
-- @
--
-- For shallow schemas:
-- @
-- completeAlg :: ShallowSchema -> [Text]
-- @
type CompleteAlg = ShallowSchema -> [Text]

-- | Collect all names at current level (methods + children)
completeAlg :: CompleteAlg
completeAlg PluginSchemaF{..} = concat
  [ map methodName psfMethods
  , maybe [] (map csNamespace) psfChildren
  ]

-- | Collect only method names
completeMethodsAlg :: CompleteAlg
completeMethodsAlg PluginSchemaF{..} = map methodName psfMethods

-- | Collect only child namespaces
completeChildrenAlg :: CompleteAlg
completeChildrenAlg PluginSchemaF{..} =
  maybe [] (map csNamespace) psfChildren

-- | Get completions for a schema (convenience wrapper)
completions :: PluginSchema -> [Text]
completions = completeAlg . toShallow

-- | Get method completions
completeMethods :: PluginSchema -> [Text]
completeMethods = completeMethodsAlg . toShallow

-- | Get child namespace completions
completeChildren :: PluginSchema -> [Text]
completeChildren = completeChildrenAlg . toShallow
