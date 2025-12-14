-- | JSON-RPC types for communicating with the Plexus
module Plexus.Types
  ( -- * JSON-RPC Protocol
    RpcRequest(..)
  , RpcResponse(..)
  , RpcError(..)
  , SubscriptionNotification(..)
  , SubNotifParams(..)
  , RequestId(..)
  , SubscriptionId(..)

    -- * Plexus Stream Types
  , PlexusStreamItem(..)
  , Provenance(..)

    -- * Helpers
  , mkSubscribeRequest
  , mkUnsubscribeRequest
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)

-- | Request ID for JSON-RPC calls
newtype RequestId = RequestId { unRequestId :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Subscription ID returned by the server (can be string or number)
newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

instance FromJSON SubscriptionId where
  parseJSON (String s) = pure $ SubscriptionId s
  parseJSON v =
    -- Store raw JSON representation as the ID
    pure $ SubscriptionId $ T.decodeUtf8 $ LBS.toStrict $ encode v

-- | JSON-RPC 2.0 request
data RpcRequest = RpcRequest
  { rpcReqJsonrpc :: Text           -- ^ Always "2.0"
  , rpcReqMethod  :: Text           -- ^ Method name (e.g., "bash_execute")
  , rpcReqParams  :: Value          -- ^ Parameters (array or object)
  , rpcReqId      :: RequestId      -- ^ Request identifier
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RpcRequest where
  toJSON RpcRequest{..} = object
    [ "jsonrpc" .= rpcReqJsonrpc
    , "method"  .= rpcReqMethod
    , "params"  .= rpcReqParams
    , "id"      .= rpcReqId
    ]

-- | JSON-RPC 2.0 response
data RpcResponse
  = RpcSuccess
      { rpcRespId     :: RequestId
      , rpcRespResult :: Value
      }
  | RpcError
      { rpcRespId    :: RequestId
      , rpcRespError :: RpcError
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON RpcResponse where
  parseJSON = withObject "RpcResponse" $ \o -> do
    rid <- o .: "id"
    mResult <- o .:? "result"
    mError  <- o .:? "error"
    case (mResult, mError) of
      (Just r, Nothing) -> pure $ RpcSuccess rid r
      (Nothing, Just e) -> pure $ RpcError rid e
      _ -> fail "Expected either 'result' or 'error' field"

-- | JSON-RPC error object
data RpcError = RpcErrorObj
  { errCode    :: Int
  , errMessage :: Text
  , errData    :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RpcError where
  parseJSON = withObject "RpcError" $ \o -> RpcErrorObj
    <$> o .: "code"
    <*> o .: "message"
    <*> o .:? "data"

-- | Subscription notification from server
-- This is what we receive for each stream item
data SubscriptionNotification = SubscriptionNotification
  { subNotifJsonrpc :: Text
  , subNotifMethod  :: Text           -- ^ Usually "subscription"
  , subNotifParams  :: SubNotifParams
  }
  deriving stock (Show, Eq, Generic)

data SubNotifParams = SubNotifParams
  { subParamsSubscription :: SubscriptionId
  , subParamsResult       :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubscriptionNotification where
  parseJSON = withObject "SubscriptionNotification" $ \o -> SubscriptionNotification
    <$> o .: "jsonrpc"
    <*> o .: "method"
    <*> o .: "params"

instance FromJSON SubNotifParams where
  parseJSON = withObject "SubNotifParams" $ \o -> SubNotifParams
    <$> o .: "subscription"
    <*> o .: "result"

-- | Provenance tracking nested calls through activations
newtype Provenance = Provenance { segments :: [Text] }
  deriving stock (Show, Eq, Generic)

instance FromJSON Provenance where
  parseJSON = withObject "Provenance" $ \o ->
    Provenance <$> o .: "segments"

instance ToJSON Provenance where
  toJSON (Provenance segs) = object ["segments" .= segs]

-- | Unified stream item from the plexus
data PlexusStreamItem
  = StreamProgress
      { itemProvenance  :: Provenance
      , itemMessage     :: Text
      , itemPercentage  :: Maybe Double
      }
  | StreamData
      { itemProvenance  :: Provenance
      , itemContentType :: Text
      , itemData        :: Value
      }
  | StreamError
      { itemProvenance  :: Provenance
      , itemError       :: Text
      , itemRecoverable :: Bool
      }
  | StreamDone
      { itemProvenance :: Provenance
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusStreamItem where
  parseJSON = withObject "PlexusStreamItem" $ \o -> do
    event <- o .: "event" :: Parser Text
    dataObj <- o .: "data"
    case event of
      "progress" -> withObject "Progress" parseProgress dataObj
      "data"     -> withObject "Data" parseData dataObj
      "error"    -> withObject "Error" parseError dataObj
      "done"     -> withObject "Done" parseDone dataObj
      _          -> fail $ "Unknown event type: " <> show event
    where
      parseProgress o = StreamProgress
        <$> o .: "provenance"
        <*> o .: "message"
        <*> o .:? "percentage"

      parseData o = StreamData
        <$> o .: "provenance"
        <*> o .: "content_type"
        <*> o .: "data"

      parseError o = StreamError
        <$> o .: "provenance"
        <*> o .: "error"
        <*> o .: "recoverable"

      parseDone o = StreamDone
        <$> o .: "provenance"

instance ToJSON PlexusStreamItem where
  toJSON (StreamProgress prov msg pct) = object
    [ "event" .= ("progress" :: Text)
    , "data"  .= object
        [ "provenance" .= prov
        , "message" .= msg
        , "percentage" .= pct
        ]
    ]
  toJSON (StreamData prov ct dat) = object
    [ "event" .= ("data" :: Text)
    , "data"  .= object
        [ "provenance" .= prov
        , "content_type" .= ct
        , "data" .= dat
        ]
    ]
  toJSON (StreamError prov err rec) = object
    [ "event" .= ("error" :: Text)
    , "data"  .= object
        [ "provenance" .= prov
        , "error" .= err
        , "recoverable" .= rec
        ]
    ]
  toJSON (StreamDone prov) = object
    [ "event" .= ("done" :: Text)
    , "data"  .= object ["provenance" .= prov]
    ]

-- | Create a subscription request
mkSubscribeRequest :: RequestId -> Text -> Value -> RpcRequest
mkSubscribeRequest rid method params = RpcRequest
  { rpcReqJsonrpc = "2.0"
  , rpcReqMethod  = method
  , rpcReqParams  = params
  , rpcReqId      = rid
  }

-- | Create an unsubscribe request
mkUnsubscribeRequest :: RequestId -> Text -> SubscriptionId -> RpcRequest
mkUnsubscribeRequest rid unsubMethod subId = RpcRequest
  { rpcReqJsonrpc = "2.0"
  , rpcReqMethod  = unsubMethod
  , rpcReqParams  = toJSON [unSubscriptionId subId]
  , rpcReqId      = rid
  }
