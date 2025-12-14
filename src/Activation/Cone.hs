-- | Typed API for the Cone activation - growth and exploration
--
-- The cone (growth cone) is the exploring tip of a growing nerve fiber.
-- It senses context, probes directions via LLM, and extends the arbor.
-- The cone is growth machinery - it doesn't decide *what* to explore,
-- the client (real agent) directs it.
module Activation.Cone
  ( -- * Re-exports
    PlexusConnection
    -- * Types
  , ConeEvent(..)
  , ConeId
  , MessageId
  , MessageRole(..)
  , Position(..)
  , ConeConfig(..)
  , ConeInfo(..)
  , ChatUsage(..)

    -- * Cone Operations
  , coneCreate
  , coneGet
  , coneList
  , coneDelete
  , coneChat
  , coneSetHead
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import Plexus.Client (PlexusConnection, plexusRpc)
import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- Core Types
-- ============================================================================

-- | UUID string identifier for cones
type ConeId = Text

-- | UUID string identifier for messages
type MessageId = Text

-- | Role of a message sender
data MessageRole
  = User
  | Assistant
  | System
  deriving stock (Show, Eq, Generic)

instance FromJSON MessageRole where
  parseJSON = withText "MessageRole" $ \case
    "user"      -> pure User
    "assistant" -> pure Assistant
    "system"    -> pure System
    other       -> fail $ "Unknown message role: " <> T.unpack other

instance ToJSON MessageRole where
  toJSON User      = String "user"
  toJSON Assistant = String "assistant"
  toJSON System    = String "system"

-- | A position in the arbor - couples tree_id and node_id together
data Position = Position
  { positionTreeId :: Text
  , positionNodeId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o ->
    Position
      <$> o .: "tree_id"
      <*> o .: "node_id"

instance ToJSON Position where
  toJSON Position{..} = object
    [ "tree_id" .= positionTreeId
    , "node_id" .= positionNodeId
    ]

-- | Token usage information
data ChatUsage = ChatUsage
  { usageInputTokens  :: Maybe Int
  , usageOutputTokens :: Maybe Int
  , usageTotalTokens  :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ChatUsage where
  parseJSON = withObject "ChatUsage" $ \o ->
    ChatUsage
      <$> o .:? "input_tokens"
      <*> o .:? "output_tokens"
      <*> o .:? "total_tokens"

instance ToJSON ChatUsage where
  toJSON ChatUsage{..} = object
    [ "input_tokens"  .= usageInputTokens
    , "output_tokens" .= usageOutputTokens
    , "total_tokens"  .= usageTotalTokens
    ]

-- | Full cone configuration
data ConeConfig = ConeConfig
  { coneConfigId           :: ConeId
  , coneConfigName         :: Text
  , coneConfigModelId      :: Text
  , coneConfigSystemPrompt :: Maybe Text
  , coneConfigHead         :: Position
  , coneConfigMetadata     :: Maybe Value
  , coneConfigCreatedAt    :: Int
  , coneConfigUpdatedAt    :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ConeConfig where
  parseJSON = withObject "ConeConfig" $ \o ->
    ConeConfig
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "model_id"
      <*> o .:? "system_prompt"
      <*> o .: "head"
      <*> o .:? "metadata"
      <*> o .: "created_at"
      <*> o .: "updated_at"

-- | Lightweight cone info (for listing)
data ConeInfo = ConeInfo
  { coneInfoId        :: ConeId
  , coneInfoName      :: Text
  , coneInfoModelId   :: Text
  , coneInfoHead      :: Position
  , coneInfoCreatedAt :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ConeInfo where
  parseJSON = withObject "ConeInfo" $ \o ->
    ConeInfo
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "model_id"
      <*> o .: "head"
      <*> o .: "created_at"

-- ============================================================================
-- Stream Events
-- ============================================================================

-- | Events emitted by Cone operations
data ConeEvent
  = ConeCreated
      { eventConeId :: ConeId
      , eventHead   :: Position
      }
  | ConeDeleted
      { eventConeId :: ConeId
      }
  | ConeUpdated
      { eventConeId :: ConeId
      }
  | ConeData
      { eventCone :: ConeConfig
      }
  | ConeList
      { eventCones :: [ConeInfo]
      }
  | ChatStart
      { eventConeId       :: ConeId
      , eventUserPosition :: Position
      }
  | ChatContent
      { eventConeId  :: ConeId
      , eventContent :: Text
      }
  | ChatComplete
      { eventConeId  :: ConeId
      , eventNewHead :: Position
      , eventUsage   :: Maybe ChatUsage
      }
  | HeadUpdated
      { eventConeId  :: ConeId
      , eventOldHead :: Position
      , eventNewHead :: Position
      }
  | ConeError
      { eventMessage :: Text
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON ConeEvent where
  parseJSON = withObject "ConeEvent" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "cone_created"  -> ConeCreated <$> o .: "cone_id" <*> o .: "head"
      "cone_deleted"  -> ConeDeleted <$> o .: "cone_id"
      "cone_updated"  -> ConeUpdated <$> o .: "cone_id"
      "cone_data"     -> ConeData <$> o .: "cone"
      "cone_list"     -> ConeList <$> o .: "cones"
      "chat_start"    -> ChatStart <$> o .: "cone_id" <*> o .: "user_position"
      "chat_content"  -> ChatContent <$> o .: "cone_id" <*> o .: "content"
      "chat_complete" -> ChatComplete <$> o .: "cone_id" <*> o .: "new_head" <*> o .:? "usage"
      "head_updated"  -> HeadUpdated <$> o .: "cone_id" <*> o .: "old_head" <*> o .: "new_head"
      "error"         -> ConeError <$> o .: "message"
      _               -> fail $ "Unknown cone event type: " <> T.unpack typ

-- ============================================================================
-- Helper
-- ============================================================================

extractConeEvent :: PlexusStreamItem -> Maybe ConeEvent
extractConeEvent (StreamData _ contentType dat)
  | contentType == "cone.event" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractConeEvent _ = Nothing

-- ============================================================================
-- Cone Operations
-- ============================================================================

-- | Create a new cone with a fresh conversation tree
coneCreate
  :: PlexusConnection
  -> Text           -- ^ Name
  -> Text           -- ^ Model ID (e.g., "gpt-4o-mini", "claude-3-haiku-20240307")
  -> Maybe Text     -- ^ System prompt
  -> Maybe Value    -- ^ Metadata
  -> Stream (Of ConeEvent) IO ()
coneCreate conn name modelId systemPrompt metadata =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_create" params
  where
    -- params: [name, model_id, system_prompt | null, metadata | null]
    params = toJSON [toJSON name, toJSON modelId, toJSON systemPrompt, toJSON metadata]

-- | Get a cone by ID
coneGet :: PlexusConnection -> ConeId -> Stream (Of ConeEvent) IO ()
coneGet conn coneId =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_get" (toJSON [coneId])

-- | List all cones
coneList :: PlexusConnection -> Stream (Of ConeEvent) IO ()
coneList conn =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_list" (toJSON ([] :: [Value]))

-- | Delete a cone (tree is preserved)
coneDelete :: PlexusConnection -> ConeId -> Stream (Of ConeEvent) IO ()
coneDelete conn coneId =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_delete" (toJSON [coneId])

-- | Extend via LLM - streams response and advances position in arbor
-- The growth cone senses context, probes via LLM, and extends
-- Returns ChatStart, multiple ChatContent events, then ChatComplete
coneChat
  :: PlexusConnection
  -> ConeId
  -> Text           -- ^ User prompt
  -> Stream (Of ConeEvent) IO ()
coneChat conn coneId prompt =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_chat" (toJSON [coneId, prompt])

-- | Move cone's head position to a different node (retraction/branching)
coneSetHead
  :: PlexusConnection
  -> ConeId
  -> Text           -- ^ Node ID to move to
  -> Stream (Of ConeEvent) IO ()
coneSetHead conn coneId nodeId =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_set_head" (toJSON [coneId, nodeId])
