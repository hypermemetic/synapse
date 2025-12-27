-- | Core types for Synapse
--
-- Re-exports schema types from substrate-protocol and defines local types.
module Synapse.Schema.Types
  ( -- * Schema Types (from substrate-protocol)
    PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)

    -- * Query helpers
  , pluginChildren
  , childNamespaces
  , isHub
  , isLeaf

    -- * Navigation Types
  , Path
  , SchemaView(..)
  , NavError(..)

    -- * Stream Types
  , StreamMeta(..)
  ) where

import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- Re-export from substrate-protocol
import Plexus.Schema.Recursive
  ( PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)
  , pluginChildren
  , childNamespaces
  , isHub
  , isLeaf
  )

-- | A path through the plugin tree (sequence of namespace segments)
type Path = [Text]

-- | A position in the schema tree after navigation
data SchemaView
  = ViewPlugin PluginSchema Path   -- ^ Landed on a plugin, path taken to get here
  | ViewMethod MethodSchema Path   -- ^ Landed on a method, path taken to get here
  deriving stock (Show, Eq)

-- | Navigation errors
data NavError
  = NotFound Text Path             -- ^ Segment not found at path
  | MethodNotTerminal Text Path    -- ^ Method with trailing path segments
  | Cycle PluginHash Path          -- ^ Cycle detected (hash seen before)
  | FetchError Text Path           -- ^ Failed to fetch schema
  deriving stock (Show, Eq)

-- | Metadata from stream events
data StreamMeta = StreamMeta
  { smProvenance :: [Text]
  , smHash       :: PluginHash
  , smTimestamp  :: Int64
  }
  deriving stock (Show, Eq, Generic)
