-- | Schema caching by content hash
--
-- Schemas are cached by their hash. Same hash = same content.
-- This enables safe caching: if the hash matches, use cached value.
module Synapse.Cache
  ( -- * Cache Operations (re-exported from Monad)
    lookupCache
  , insertCache

    -- * Cache-aware fetching
  , fetchCached
  ) where

import Synapse.Schema.Types
import Synapse.Monad

-- | Fetch a schema, using cache if available
-- The fetcher function is only called if not in cache
fetchCached :: PluginHash -> SynapseM PluginSchema -> SynapseM PluginSchema
fetchCached hash fetcher = do
  cached <- lookupCache hash
  case cached of
    Just schema -> pure schema
    Nothing -> do
      schema <- fetcher
      insertCache (psHash schema) schema
      pure schema
