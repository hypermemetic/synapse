-- | Plexus RPC Client
--
-- The plexus is the nerve center that routes calls and coordinates activations.
-- This module provides the core primitive for communicating with the plexus
-- over JSON-RPC WebSocket.
--
-- = Quick Start
--
-- @
-- import Plexus
-- import qualified Streaming.Prelude as S
-- import Data.Aeson (toJSON)
--
-- main :: IO ()
-- main = do
--   -- Connect to plexus
--   conn <- connect defaultConfig
--
--   -- Call bash.execute and stream results
--   S.print $ plexusRpc conn "bash_execute" (toJSON ["echo hello"])
--
--   -- Clean up
--   disconnect conn
-- @
--
-- = Architecture
--
-- The client is built around a single core primitive:
--
-- @
-- plexusRpc :: PlexusConnection -> Text -> Value -> Stream (Of PlexusStreamItem) IO ()
-- @
--
-- This function:
--
-- 1. Sends a JSON-RPC subscription request
-- 2. Receives the subscription ID
-- 3. Yields 'PlexusStreamItem' values as they arrive
-- 4. Completes when 'StreamDone' or 'StreamError' is received
--
-- = Stream Items
--
-- The plexus returns a unified stream type with four variants:
--
-- * 'StreamProgress' - Progress updates with optional percentage
-- * 'StreamData' - Actual data with content type information
-- * 'StreamError' - Error (may be recoverable)
-- * 'StreamDone' - Stream completed successfully
--
module Plexus
  ( -- * Connection
    PlexusConnection
  , PlexusConfig(..)
  , defaultConfig
  , connect
  , disconnect

    -- * Core RPC
  , plexusRpc

    -- * Stream Types
  , PlexusStreamItem(..)
  , Provenance(..)

    -- * JSON-RPC Types
  , RpcRequest(..)
  , RpcResponse(..)
  , RpcError(..)
  , RequestId(..)
  , SubscriptionId(..)
  ) where

import Plexus.Types
import Plexus.Client
