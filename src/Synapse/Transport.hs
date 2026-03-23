{-# LANGUAGE ScopedTypeVariables #-}

-- | Transport layer for Synapse
--
-- Bridges the low-level Substrate transport to the SynapseM monad.
module Synapse.Transport
  ( -- * Schema Fetching
    fetchSchema
  , fetchSchemaAt
  , fetchMethodSchema

    -- * Method Invocation (collected)
  , invoke
  , invokeRaw

    -- * Method Invocation (streaming)
  , invokeStreaming
  , invokeStreamingWithBidir

    -- * Bidirectional Response
  , sendResponse
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

import Plexus.Client (SubstrateConfig(..))
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT

import Synapse.Schema.Types
import Synapse.Monad
import qualified Synapse.Log as Log

-- | Fetch the root schema
fetchSchema :: SynapseM PluginSchema
fetchSchema = do
  logger <- getLogger
  Log.logInfo logger Log.SubsystemSchema "Fetching root schema"
  fetchSchemaAt []

-- | Convert plexus-protocol TransportError to error message
transportErrorToText :: PT.TransportError -> Text
transportErrorToText (PT.ConnectionRefused host port) =
  "Connection refused to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ConnectionTimeout host port) =
  "Connection timeout to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ProtocolError msg) = "Protocol error: " <> msg
transportErrorToText (PT.NetworkError msg) = "Network error: " <> msg

-- | Convert plexus-protocol TransportError to TransportCategory
transportErrorToCategory :: PT.TransportError -> TransportErrorCategory
transportErrorToCategory (PT.ConnectionRefused _ _) = ConnectionRefused
transportErrorToCategory (PT.ConnectionTimeout _ _) = ConnectionTimeout
transportErrorToCategory (PT.ProtocolError _) = ProtocolError
transportErrorToCategory (PT.NetworkError _) = UnknownTransportError

-- | Fetch schema at a specific path
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  logger <- getLogger
  cfg <- getConfig
  Log.logWith logger Log.Debug Log.SubsystemSchema "Fetching schema at path"
    [("path", T.pack $ show path), ("host", T.pack $ substrateHost cfg), ("port", T.pack $ show $ substratePort cfg)]
  result <- liftIO $ ST.fetchSchemaAt cfg path
  case result of
    Left err -> do
      Log.logWith logger Log.Info Log.SubsystemSchema "Schema fetch failed"
        [("path", T.pack $ show path), ("error", transportErrorToText err)]
      throwNav $ FetchError (transportErrorToText err) path
    Right schema -> do
      Log.logWith logger Log.Debug Log.SubsystemSchema "Schema fetch succeeded"
        [("path", T.pack $ show path), ("namespace", psNamespace schema)]
      pure schema

-- | Fetch a specific method's schema (more efficient than full plugin schema)
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchema :: Path -> Text -> SynapseM MethodSchema
fetchMethodSchema path methodName = do
  cfg <- getConfig
  result <- liftIO $ ST.fetchMethodSchemaAt cfg path methodName
  case result of
    Left err -> throwNav $ FetchError (transportErrorToText err) path
    Right schema -> pure schema

-- | Invoke a method and return stream items
invoke :: Path -> Text -> Value -> SynapseM [HubStreamItem]
invoke namespacePath method params = do
  logger <- getLogger
  cfg <- getConfig
  backend <- asks seBackend
  let fullPath = namespacePath ++ [method]
  Log.logWith logger Log.Info Log.SubsystemRPC "Invoking RPC method"
    [ ("method", T.intercalate "." fullPath)
    , ("backend", backend)
    , ("host", T.pack $ substrateHost cfg)
    , ("port", T.pack $ show $ substratePort cfg)
    ]
  Log.logWith logger Log.Trace Log.SubsystemRPC "RPC request params"
    [("params", T.pack $ show params)]
  result <- liftIO $ ST.invokeMethod cfg namespacePath method params
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      Log.logWith logger Log.Info Log.SubsystemRPC "RPC invocation failed"
        [ ("method", T.intercalate "." fullPath)
        , ("error", transportErrorToText transportErr)
        , ("category", T.pack $ show $ transportErrorToCategory transportErr)
        ]
      throwTransportWith ctx
    Right items -> do
      Log.logWith logger Log.Debug Log.SubsystemRPC "RPC invocation succeeded"
        [ ("method", T.intercalate "." fullPath)
        , ("items_count", T.pack $ show $ length items)
        ]
      pure items
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke with raw method path
invokeRaw :: Text -> Value -> SynapseM [HubStreamItem]
invokeRaw method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeRaw cfg method params
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      backend <- asks seBackend
      let path = T.splitOn "." method
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right items -> pure items
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke a method with streaming output - calls callback for each item
invokeStreaming :: Path -> Text -> Value -> (HubStreamItem -> IO ()) -> SynapseM ()
invokeStreaming namespacePath method params onItem = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethodStreaming cfg namespacePath method params onItem
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke a method with streaming output and bidirectional request handling
-- When a StreamRequest is received, the bidirectional handler is called and
-- the response is sent back via {backend}.respond (if the handler returns Just).
-- If the handler returns Nothing (BidirRespond mode), no response is sent immediately.
invokeStreamingWithBidir
  :: Path
  -> Text
  -> Value
  -> (HubStreamItem -> IO ())                              -- ^ Handler for non-request items
  -> (Text -> PT.Request Value -> IO (Maybe (PT.Response Value)))  -- ^ Handler for bidirectional requests
  -> SynapseM ()
invokeStreamingWithBidir namespacePath method params onItem onBidirRequest = do
  cfg <- getConfig
  let wrappedHandler item = case item of
        PT.StreamRequest _ _ reqId reqData _timeout -> do
          -- Handle the bidirectional request
          mResponse <- onBidirRequest reqId reqData
          -- Send the response back via {backend}.respond (if present)
          case mResponse of
            Just response -> do
              _ <- ST.sendBidirectionalResponse cfg reqId response
              pure ()
            Nothing -> pure ()  -- agent will respond via separate call
        _ -> onItem item
  result <- liftIO $ ST.invokeMethodStreaming cfg namespacePath method params wrappedHandler
  case result of
    Left transportErr -> do
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Send a response for a bidirectional request
sendResponse :: Text -> PT.Response Value -> SynapseM ()
sendResponse requestId response = do
  cfg <- getConfig
  result <- liftIO $ ST.sendBidirectionalResponse cfg requestId response
  case result of
    Left transportErr -> do
      backend <- asks seBackend
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = T.pack $ substrateHost cfg
            , tcPort     = substratePort cfg
            , tcBackend  = backend
            , tcPath     = ["respond"]
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()

-- | Get SubstrateConfig from environment
getConfig :: SynapseM SubstrateConfig
getConfig = do
  host <- asks seHost
  port <- asks sePort
  backend <- asks seBackend
  pure $ SubstrateConfig
    { substrateHost = T.unpack host
    , substratePort = port
    , substratePath = "/"
    , substrateBackend = backend
    }
