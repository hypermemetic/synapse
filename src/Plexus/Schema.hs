-- | Schema types for dynamic CLI discovery
--
-- These types mirror the substrate's PlexusSchema and ActivationInfo,
-- allowing the CLI to discover available commands at runtime.
module Plexus.Schema
  ( -- * Schema Types
    PlexusSchema(..)
  , ActivationInfo(..)
  , PlexusSchemaEvent(..)
    -- * Method Schema (for enriched schemas)
  , MethodSchema(..)
  , ParamSchema(..)
  , ParamType(..)
    -- * Helpers
  , extractSchemaEvent
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- Core Schema Types (from plexus_schema subscription)
-- ============================================================================

-- | Information about a single activation
data ActivationInfo = ActivationInfo
  { activationNamespace   :: Text   -- ^ e.g., "arbor", "cone", "health"
  , activationVersion     :: Text   -- ^ Semantic version e.g., "1.0.0"
  , activationDescription :: Text   -- ^ Human-readable description
  , activationMethods     :: [Text] -- ^ Method names e.g., ["tree_create", "tree_get"]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ActivationInfo where
  parseJSON = withObject "ActivationInfo" $ \o ->
    ActivationInfo
      <$> o .: "namespace"
      <*> o .: "version"
      <*> o .: "description"
      <*> o .: "methods"

instance ToJSON ActivationInfo where
  toJSON ActivationInfo{..} = object
    [ "namespace"   .= activationNamespace
    , "version"     .= activationVersion
    , "description" .= activationDescription
    , "methods"     .= activationMethods
    ]

-- | Top-level schema returned by plexus_schema subscription
data PlexusSchema = PlexusSchema
  { schemaActivations  :: [ActivationInfo]
  , schemaTotalMethods :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusSchema where
  parseJSON = withObject "PlexusSchema" $ \o ->
    PlexusSchema
      <$> o .: "activations"
      <*> o .: "total_methods"

instance ToJSON PlexusSchema where
  toJSON PlexusSchema{..} = object
    [ "activations"   .= schemaActivations
    , "total_methods" .= schemaTotalMethods
    ]

-- | Event wrapper for plexus schema stream
data PlexusSchemaEvent
  = SchemaData PlexusSchema
  | SchemaError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusSchemaEvent where
  parseJSON = withObject "PlexusSchemaEvent" $ \o -> do
    mTyp <- o .:? "type"
    case mTyp :: Maybe Text of
      Just "error" -> SchemaError <$> o .: "message"
      _ -> do
        -- Schema data comes directly without type wrapper
        mActivations <- o .:? "activations" :: Parser (Maybe Value)
        case mActivations of
          Just _ -> SchemaData <$> parseJSON (Object o)
          Nothing -> fail "PlexusSchemaEvent: expected 'activations' field"

-- ============================================================================
-- Method Schema Types (for future enriched schema support)
-- ============================================================================

-- | Parameter type enumeration
data ParamType
  = ParamString
  | ParamInteger
  | ParamNumber
  | ParamBoolean
  | ParamObject
  | ParamArray ParamType
  | ParamNullable ParamType
  deriving stock (Show, Eq, Generic)

-- | Schema for a single parameter
data ParamSchema = ParamSchema
  { paramName     :: Text
  , paramType     :: ParamType
  , paramFormat   :: Maybe Text   -- ^ e.g., "uuid"
  , paramRequired :: Bool
  , paramDesc     :: Maybe Text
  , paramDefault  :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

-- | Schema for a method with full param details
data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Maybe Text
  , methodParams      :: [ParamSchema]
  }
  deriving stock (Show, Eq, Generic)

-- | Parse ParamType from JSON Schema type field
parseParamType :: Value -> ParamType
parseParamType (String "string")  = ParamString
parseParamType (String "integer") = ParamInteger
parseParamType (String "number")  = ParamNumber
parseParamType (String "boolean") = ParamBoolean
parseParamType (String "object")  = ParamObject
parseParamType (Array types)      =
  -- Handle nullable types like ["string", "null"]
  let typeList = foldr (:) [] types
  in case filter (/= String "null") typeList of
    [t] -> ParamNullable (parseParamType t)
    _   -> ParamObject  -- fallback
parseParamType _                  = ParamObject  -- fallback

-- ============================================================================
-- Stream Helpers
-- ============================================================================

-- | Extract PlexusSchemaEvent from a stream item
-- The plexus_schema subscription uses content_type "plexus.schema"
extractSchemaEvent :: PlexusStreamItem -> Maybe PlexusSchemaEvent
extractSchemaEvent (StreamData _ contentType dat)
  | contentType == "plexus.schema" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractSchemaEvent _ = Nothing
