{-# LANGUAGE ScopedTypeVariables #-}

-- | Transport layer for Plexus RPC calls
--
-- Handles WebSocket communication with the Plexus backend.
-- All calls go through 'plexus_call' for routing.
module Synapse.Transport
  ( -- * Schema Fetching
    fetchSchema
  , fetchSchemaAt

    -- * Method Invocation
  , invoke
  , invokeRaw

    -- * Low-level
  , rpcCall
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (PlexusConfig(..), plexusRpc)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema.Recursive (parsePluginSchema)

import Synapse.Schema.Types
import Synapse.Monad

-- | Fetch the root schema
fetchSchema :: SynapseM PluginSchema
fetchSchema = fetchSchemaAt []

-- | Fetch schema at a specific path
-- Empty path = root (plexus.schema)
-- Non-empty path = child schema (e.g., ["solar", "earth"] -> solar.earth.schema)
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  let schemaMethod = if null path
        then "plexus.schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCall "plexus_call" (object ["method" .= schemaMethod])
  case result of
    Left err -> throwTransport err
    Right items -> extractSchema path items

-- | Extract PluginSchema from stream items
extractSchema :: Path -> [PlexusStreamItem] -> SynapseM PluginSchema
extractSchema path items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> case parsePluginSchema dat of
      Right schema -> pure schema
      Left err -> throwParse err
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> throwNav $ FetchError err path
      [] -> throwTransport "No schema in response"

-- | Invoke a method and return stream items
invoke :: Path -> Text -> Value -> SynapseM [PlexusStreamItem]
invoke namespacePath method params = do
  let fullPath = if null namespacePath then ["plexus"] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  result <- rpcCall "plexus_call" callParams
  case result of
    Left err -> throwTransport err
    Right items -> pure items

-- | Invoke and return raw stream items (for streaming output)
invokeRaw :: Text -> Value -> SynapseM [PlexusStreamItem]
invokeRaw method params = do
  let callParams = object ["method" .= method, "params" .= params]
  result <- rpcCall "plexus_call" callParams
  case result of
    Left err -> throwTransport err
    Right items -> pure items

-- | Low-level RPC call, returns collected stream items or error
rpcCall :: Text -> Value -> SynapseM (Either Text [PlexusStreamItem])
rpcCall method params = do
  host <- asks seHost
  port <- asks sePort
  let cfg = defaultConfig { plexusHost = T.unpack host, plexusPort = port }
  liftIO $ doCall cfg method params

doCall :: PlexusConfig -> Text -> Value -> IO (Either Text [PlexusStreamItem])
doCall cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` \(e :: SomeException) ->
      pure $ Left $ T.pack $ "Connection error: " <> show e
  pure result

doCallInner :: PlexusConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  conn <- connect cfg
  items <- S.toList_ $ plexusRpc conn method params
  disconnect conn
  pure items
