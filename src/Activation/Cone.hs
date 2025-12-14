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
    -- * Registry Types
  , RegistryExport(..)
  , ServiceExport(..)
  , ModelExport(..)
  , ModelCapabilities(..)
  , ModelPricing(..)
  , RegistryStats(..)

    -- * Cone Operations
  , coneCreate
  , coneGet
  , coneList
  , coneDelete
  , coneChat
  , coneSetHead
  , coneRegistry
  ) where

import Data.Aeson
import Data.Aeson.Types (Object, Parser)
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

-- ============================================================================
-- Registry Types (from cllient::ModelRegistry)
-- ============================================================================

-- | Model capabilities
data ModelCapabilities = ModelCapabilities
  { capContextWindow   :: Maybe Int
  , capMaxOutputTokens :: Maybe Int
  , capVision          :: Maybe Bool
  , capStreaming       :: Maybe Bool
  , capFunctionCalling :: Maybe Bool
  , capJsonMode        :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelCapabilities where
  parseJSON = withObject "ModelCapabilities" $ \o ->
    ModelCapabilities
      <$> o .:? "context_window"
      <*> o .:? "max_output_tokens"
      <*> o .:? "vision"
      <*> o .:? "streaming"
      <*> o .:? "function_calling"
      <*> o .:? "json_mode"

-- | Model pricing per 1k tokens
data ModelPricing = ModelPricing
  { pricingInputPer1k  :: Maybe Double
  , pricingOutputPer1k :: Maybe Double
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelPricing where
  parseJSON = withObject "ModelPricing" $ \o ->
    ModelPricing
      <$> o .:? "input_per_1k_tokens"
      <*> o .:? "output_per_1k_tokens"

-- | Exported model info
data ModelExport = ModelExport
  { modelId           :: Text
  , modelFamily       :: Text
  , modelService      :: Text
  , modelCapabilities :: ModelCapabilities
  , modelPricing      :: ModelPricing
  , modelStatus       :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ModelExport where
  parseJSON = withObject "ModelExport" $ \o ->
    ModelExport
      <$> o .: "id"
      <*> o .: "family"
      <*> o .: "service"
      <*> o .: "capabilities"
      <*> o .: "pricing"
      <*> o .: "status"

-- | Exported service info
data ServiceExport = ServiceExport
  { serviceName           :: Text
  , serviceBaseUrl        :: Maybe Text
  , serviceMessageBuilder :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ServiceExport where
  parseJSON = withObject "ServiceExport" $ \o ->
    ServiceExport
      <$> o .: "name"
      <*> o .:? "base_url"
      <*> o .:? "message_builder"

-- | Registry statistics
data RegistryStats = RegistryStats
  { statsModelCount   :: Int
  , statsServiceCount :: Int
  , statsFamilyCount  :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RegistryStats where
  parseJSON = withObject "RegistryStats" $ \o ->
    RegistryStats
      <$> o .: "model_count"
      <*> o .: "service_count"
      <*> o .: "family_count"

-- | Full registry export
data RegistryExport = RegistryExport
  { registryServices :: [ServiceExport]
  , registryFamilies :: [Text]
  , registryModels   :: [ModelExport]
  , registryStats    :: RegistryStats
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RegistryExport where
  parseJSON = withObject "RegistryExport" $ \o ->
    RegistryExport
      <$> o .: "services"
      <*> o .: "families"
      <*> o .: "models"
      <*> o .: "stats"

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
  | RegistryData
      { eventRegistry :: RegistryExport
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON ConeEvent where
  parseJSON = withObject "ConeEvent" $ \o -> do
    mTyp <- o .:? "type"
    case mTyp :: Maybe Text of
      Just "cone_created"  -> ConeCreated <$> o .: "cone_id" <*> o .: "head"
      Just "cone_deleted"  -> ConeDeleted <$> o .: "cone_id"
      Just "cone_updated"  -> ConeUpdated <$> o .: "cone_id"
      Just "cone_data"     -> ConeData <$> o .: "cone"
      Just "cone_list"     -> ConeList <$> o .: "cones"
      Just "chat_start"    -> ChatStart <$> o .: "cone_id" <*> o .: "user_position"
      Just "chat_content"  -> ChatContent <$> o .: "cone_id" <*> o .: "content"
      Just "chat_complete" -> ChatComplete <$> o .: "cone_id" <*> o .: "new_head" <*> o .:? "usage"
      Just "head_updated"  -> HeadUpdated <$> o .: "cone_id" <*> o .: "old_head" <*> o .: "new_head"
      Just "error"         -> ConeError <$> o .: "message"
      Just "registry"      -> RegistryData <$> parseJSON (Object o)
      Just other           -> fail $ "Unknown cone event type: " <> T.unpack other
      -- Registry data comes without a type field - detect by presence of services/models/families
      Nothing -> do
        mServices <- o .:? "services" :: Parser (Maybe Value)
        case mServices of
          Just _ -> RegistryData <$> parseJSON (Object o)
          Nothing -> fail "ConeEvent missing 'type' field and not a registry"

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

-- | Get the model registry (available services, families, models)
coneRegistry :: PlexusConnection -> Stream (Of ConeEvent) IO ()
coneRegistry conn =
  S.mapMaybe extractConeEvent $
    plexusRpc conn "cone_registry" (toJSON ([] :: [Value]))
