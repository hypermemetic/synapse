{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Backend Discovery Abstraction
--
-- This module provides an abstraction layer for discovering Plexus backends.
-- Currently implements a stub that returns hardcoded localhost:4444, but the
-- interface is designed to support future dynamic discovery via a directory
-- service.
--
-- = Future Vision
--
-- @
-- ┌─────────────┐     ┌──────────────────┐     ┌─────────────────┐
-- │   synapse   │────▶│  Hub Directory   │────▶│  Backend Hubs   │
-- │   (CLI)     │     │  (port 4444)     │     │  (discovered)   │
-- └─────────────┘     └──────────────────┘     └─────────────────┘
--                             │
--                             ▼
--                     Returns list of:
--                     - Hub name
--                     - Connection info (host:port or https URL)
--                     - Description
--                     - Schema hash
-- @
--
-- = Usage
--
-- @
-- main :: IO ()
-- main = do
--   let discovery = stubDiscovery
--   backends <- discoverBackends discovery
--   -- Use backends to build CLI subcommands
-- @
module Synapse.Backend.Discovery
  ( -- * Backend Type
    Backend(..)

    -- * Discovery Interface
  , BackendDiscovery(..)

    -- * Implementations
  , stubDiscovery

    -- * Future: Directory-based discovery
    -- , directoryDiscovery
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ============================================================================
-- Backend Type
-- ============================================================================

-- | A discovered backend hub
data Backend = Backend
  { backendName        :: Text       -- ^ e.g., "plexus"
  , backendDescription :: Text       -- ^ Human-readable description
  , backendHost        :: Text       -- ^ Host to connect to
  , backendPort        :: Int        -- ^ Port
  , backendVersion     :: Maybe Text -- ^ Version if known
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- ============================================================================
-- Discovery Interface
-- ============================================================================

-- | Backend discovery interface
--
-- This is a record-of-functions pattern that allows swapping discovery
-- implementations without changing calling code. In the future, this could
-- become a typeclass if we need more sophisticated dispatch.
data BackendDiscovery = BackendDiscovery
  { discoverBackends :: IO [Backend]
      -- ^ Query for all available backends
  , getBackendInfo   :: Text -> IO (Maybe Backend)
      -- ^ Get info for a specific backend by name
  }

-- ============================================================================
-- Stub Implementation
-- ============================================================================

-- | Stub discovery that returns hardcoded plexus on localhost:4444
--
-- This is the current implementation. Once we have a directory service,
-- we can swap this for 'directoryDiscovery'.
stubDiscovery :: BackendDiscovery
stubDiscovery = BackendDiscovery
  { discoverBackends = pure [defaultPlexusBackend]
  , getBackendInfo   = \name ->
      if name == "plexus"
        then pure $ Just defaultPlexusBackend
        else pure Nothing
  }

-- | The default Plexus backend configuration
defaultPlexusBackend :: Backend
defaultPlexusBackend = Backend
  { backendName        = "plexus"
  , backendDescription = "Plexus Hub"
  , backendHost        = "127.0.0.1"
  , backendPort        = 4444
  , backendVersion     = Nothing
  }

-- ============================================================================
-- Future: Directory-based Discovery
-- ============================================================================

-- | Query a directory service for available backends
--
-- Future implementation placeholder:
--
-- @
-- directoryDiscovery :: Text -> Int -> BackendDiscovery
-- directoryDiscovery dirHost dirPort = BackendDiscovery
--   { discoverBackends = queryDirectory dirHost dirPort
--   , getBackendInfo   = queryBackend dirHost dirPort
--   }
--
-- queryDirectory :: Text -> Int -> IO [Backend]
-- queryDirectory host port = do
--   -- Connect to directory service
--   -- Call directory.list or similar
--   -- Parse response into [Backend]
--   undefined
--
-- queryBackend :: Text -> Int -> Text -> IO (Maybe Backend)
-- queryBackend host port name = do
--   -- Query specific backend from directory
--   undefined
-- @
