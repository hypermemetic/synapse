-- | Schema types for dynamic CLI discovery
--
-- These types mirror the substrate's PlexusSchema and ActivationInfo,
-- allowing the CLI to discover available commands at runtime.
module Plexus.Schema
  ( -- * Schema Types
    PlexusSchema(..)
  , ActivationInfo(..)
  , PlexusSchemaEvent(..)
    -- * Enriched Schema Types
  , EnrichedSchema(..)
  , SchemaProperty(..)
  , ActivationSchemaEvent(..)
    -- * Method Schema (parsed from enriched)
  , MethodSchema(..)
  , ParamSchema(..)
  , ParamType(..)
    -- * Parsing
  , parseMethodSchemas
  , parseMethodVariantByIndex
  , parseParamType
    -- * Stream Helpers
  , extractSchemaEvent
  , extractActivationSchemaEvent
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
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
-- Enriched Schema Types (from plexus_activation_schema)
-- ============================================================================

-- | Enriched JSON Schema from substrate
-- Mirrors the Rust Schema type from plexus/schema.rs
data EnrichedSchema = EnrichedSchema
  { schemaTitle       :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaType        :: Maybe Value         -- ^ "object", "string", etc.
  , schemaProperties  :: Maybe (Map Text SchemaProperty)
  , schemaRequired    :: Maybe [Text]
  , schemaOneOf       :: Maybe [EnrichedSchema]  -- ^ Method variants
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EnrichedSchema where
  parseJSON = withObject "EnrichedSchema" $ \o ->
    EnrichedSchema
      <$> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "type"
      <*> o .:? "properties"
      <*> o .:? "required"
      <*> o .:? "oneOf"

instance ToJSON EnrichedSchema where
  toJSON EnrichedSchema{..} = object $ catMaybes
    [ ("title" .=) <$> schemaTitle
    , ("description" .=) <$> schemaDescription
    , ("type" .=) <$> schemaType
    , ("properties" .=) <$> schemaProperties
    , ("required" .=) <$> schemaRequired
    , ("oneOf" .=) <$> schemaOneOf
    ]

-- | Schema property definition
data SchemaProperty = SchemaProperty
  { propType        :: Maybe Value       -- ^ "string", "integer", ["string", "null"]
  , propDescription :: Maybe Text
  , propFormat      :: Maybe Text        -- ^ "uuid", "date-time", etc.
  , propItems       :: Maybe SchemaProperty  -- ^ For array types
  , propDefault     :: Maybe Value
  , propEnum        :: Maybe [Value]
  , propProperties  :: Maybe (Map Text SchemaProperty)  -- ^ Nested object
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SchemaProperty where
  parseJSON = withObject "SchemaProperty" $ \o ->
    SchemaProperty
      <$> o .:? "type"
      <*> o .:? "description"
      <*> o .:? "format"
      <*> o .:? "items"
      <*> o .:? "default"
      <*> o .:? "enum"
      <*> o .:? "properties"

instance ToJSON SchemaProperty where
  toJSON SchemaProperty{..} = object $ catMaybes
    [ ("type" .=) <$> propType
    , ("description" .=) <$> propDescription
    , ("format" .=) <$> propFormat
    , ("items" .=) <$> propItems
    , ("default" .=) <$> propDefault
    , ("enum" .=) <$> propEnum
    , ("properties" .=) <$> propProperties
    ]

-- | Event wrapper for activation schema stream
data ActivationSchemaEvent
  = ActivationSchemaData EnrichedSchema
  | ActivationSchemaError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON ActivationSchemaEvent where
  parseJSON val = case val of
    Object o -> do
      mErr <- o .:? "error"
      case mErr of
        Just err -> pure $ ActivationSchemaError err
        Nothing  -> ActivationSchemaData <$> parseJSON val
    _ -> fail "ActivationSchemaEvent: expected object"

-- ============================================================================
-- Method Schema Types (parsed from EnrichedSchema)
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

-- | Parse an EnrichedSchema into a list of MethodSchemas
-- The enriched schema has oneOf containing method variants
parseMethodSchemas :: EnrichedSchema -> [MethodSchema]
parseMethodSchemas schema = case schemaOneOf schema of
  Just variants -> mapMaybe parseMethodVariant variants
  Nothing -> []

-- | Parse a single method variant from the oneOf array
parseMethodVariant :: EnrichedSchema -> Maybe MethodSchema
parseMethodVariant variant = do
  props <- schemaProperties variant
  -- Get method name from "method" property's const/enum (may not exist)
  let methodProp = Map.lookup "method" props
      mMethodName = methodProp >>= extractMethodName
  -- Get params from "params" property
  let params = case Map.lookup "params" props of
        Just paramsProp -> extractParams paramsProp (schemaRequired variant)
        Nothing -> []
  -- Return schema even without method name (we'll use index-based lookup)
  pure MethodSchema
    { methodName = fromMaybe "" mMethodName
    , methodDescription = schemaDescription variant
    , methodParams = params
    }

-- | Parse a method variant by index (for when method names aren't in schema)
parseMethodVariantByIndex :: EnrichedSchema -> Int -> Maybe MethodSchema
parseMethodVariantByIndex schema idx = case schemaOneOf schema of
  Just variants | idx < length variants -> parseMethodVariant (variants !! idx)
  _ -> Nothing

-- | Extract method name from the method property
extractMethodName :: SchemaProperty -> Maybe Text
extractMethodName prop = case propEnum prop of
  Just (String name : _) -> Just name
  _ -> Nothing

-- | Extract parameters from the params property
extractParams :: SchemaProperty -> Maybe [Text] -> [ParamSchema]
extractParams paramsProp mRequired =
  case propProperties paramsProp of
    Just props -> map (toParamSchema required) (Map.toList props)
    Nothing -> []
  where
    required = fromMaybe [] mRequired

-- | Convert a property to ParamSchema
toParamSchema :: [Text] -> (Text, SchemaProperty) -> ParamSchema
toParamSchema required (name, prop) = ParamSchema
  { paramName = name
  , paramType = maybe ParamObject parseParamType (propType prop)
  , paramFormat = propFormat prop
  , paramRequired = name `elem` required
  , paramDesc = propDescription prop
  , paramDefault = propDefault prop
  }

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

-- | Extract ActivationSchemaEvent from a stream item
-- The plexus_activation_schema subscription uses content_type "plexus.activation_schema"
extractActivationSchemaEvent :: PlexusStreamItem -> Maybe ActivationSchemaEvent
extractActivationSchemaEvent (StreamData _ contentType dat)
  | contentType == "plexus.activation_schema" =
      case fromJSON dat of
        Success schema -> Just (ActivationSchemaData schema)
        Error _        -> Nothing
  | otherwise = Nothing
extractActivationSchemaEvent (StreamError _ err _) = Just (ActivationSchemaError err)
extractActivationSchemaEvent _ = Nothing
