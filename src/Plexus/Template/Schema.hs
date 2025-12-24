{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Parse JSON Schema return types into structured ADT for template generation
--
-- This module takes the "returns" field from plexus_full_schema and parses it
-- into a representation suitable for generating Mustache templates.
module Plexus.Template.Schema
  ( -- * Schema Types
    FieldSchema(..)
  , VariantSchema(..)
  , ReturnSchema(..)
  , ObjectFields
    -- * Parsing
  , parseReturnSchema
  , parseFieldSchema
    -- * Utilities
  , fieldSchemaType
  , isStreamingVariant
  , getDiscriminator
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- ============================================================================
-- Types
-- ============================================================================

-- | Schema for a single field
data FieldSchema
  = FString                              -- ^ Simple string
  | FInteger                             -- ^ Integer number
  | FNumber                              -- ^ Floating point number
  | FBoolean                             -- ^ Boolean
  | FArray FieldSchema                   -- ^ Array of elements
  | FObject ObjectFields                 -- ^ Nested object with fields
  | FNullable FieldSchema                -- ^ Nullable (type: ["X", "null"])
  | FRef Text                            -- ^ Reference to $defs (unresolved)
  | FConst Text                          -- ^ Constant value (discriminator)
  | FAny                                 -- ^ Dynamic/untyped (metadata, etc.)
  deriving stock (Show, Eq, Generic)

-- | Object fields: (name, schema, required?)
type ObjectFields = [(Text, FieldSchema, Bool)]

-- | A single variant in a discriminated union
data VariantSchema = VariantSchema
  { variantName   :: Text                -- ^ Discriminator value (e.g., "chat_content")
  , variantDesc   :: Maybe Text          -- ^ Description from schema
  , variantFields :: ObjectFields        -- ^ Fields excluding discriminator
  } deriving stock (Show, Eq, Generic)

-- | Parsed return schema
data ReturnSchema
  = RSSingle                             -- ^ Simple object return
      { rsSingleDesc   :: Maybe Text
      , rsSingleFields :: ObjectFields
      }
  | RSUnion                              -- ^ Discriminated union (oneOf with const type)
      { rsDiscriminator :: Text          -- ^ Field name used as discriminator (usually "type")
      , rsVariants      :: [VariantSchema]
      , rsDefs          :: Map Text Value -- ^ $defs for resolving references
      }
  | RSPrimitive FieldSchema              -- ^ Primitive return (string, array, etc.)
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Parsing
-- ============================================================================

-- | Parse a return schema from JSON Schema value
parseReturnSchema :: Value -> Either Text ReturnSchema
parseReturnSchema val = case val of
  Object o -> parseObjectSchema o
  _ -> Left "Return schema must be an object"

-- | Parse an object schema (may be single object or oneOf union)
parseObjectSchema :: Object -> Either Text ReturnSchema
parseObjectSchema o = do
  let defs = case KM.lookup "$defs" o of
        Just (Object d) -> Map.fromList [(keyToText k, v) | (k, v) <- KM.toList d]
        _ -> Map.empty

  case KM.lookup "oneOf" o of
    Just (Array variants) -> parseUnion defs (V.toList variants)
    _ -> parseSingleObject defs o

-- | Parse a discriminated union from oneOf variants
parseUnion :: Map Text Value -> [Value] -> Either Text ReturnSchema
parseUnion defs variants = do
  parsed <- mapM (parseVariant defs) variants
  -- Find the discriminator field (the one with const in all variants)
  let discriminator = findDiscriminator parsed
  pure $ RSUnion
    { rsDiscriminator = fromMaybe "type" discriminator
    , rsVariants = parsed
    , rsDefs = defs
    }

-- | Parse a single variant from oneOf
parseVariant :: Map Text Value -> Value -> Either Text VariantSchema
parseVariant defs (Object o) = do
  let desc = case KM.lookup "description" o of
        Just (String d) -> Just d
        _ -> Nothing

  let required = case KM.lookup "required" o of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []

  -- Parse properties
  fields <- case KM.lookup "properties" o of
    Just (Object props) -> parseProperties defs required props
    _ -> pure []

  -- Extract discriminator value (type: const "X")
  let discValue = extractDiscriminatorValue fields
  let nonDiscFields = filter (\(n, _, _) -> n /= "type") fields

  pure $ VariantSchema
    { variantName = fromMaybe "unknown" discValue
    , variantDesc = desc
    , variantFields = nonDiscFields
    }
parseVariant _ _ = Left "Variant must be an object"

-- | Parse a single object schema (not a union)
parseSingleObject :: Map Text Value -> Object -> Either Text ReturnSchema
parseSingleObject defs o = do
  let desc = case KM.lookup "description" o of
        Just (String d) -> Just d
        _ -> Nothing

  let required = case KM.lookup "required" o of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []

  fields <- case KM.lookup "properties" o of
    Just (Object props) -> parseProperties defs required props
    _ -> pure []

  pure $ RSSingle
    { rsSingleDesc = desc
    , rsSingleFields = fields
    }

-- | Parse properties into field list
parseProperties :: Map Text Value -> [Text] -> Object -> Either Text ObjectFields
parseProperties defs required props =
  sequence [ do
    schema <- parseFieldSchema defs v
    pure (keyToText k, schema, keyToText k `elem` required)
  | (k, v) <- KM.toList props
  ]

-- | Parse a field schema
parseFieldSchema :: Map Text Value -> Value -> Either Text FieldSchema
parseFieldSchema defs (Object o) = do
  -- Check for $ref first
  case KM.lookup "$ref" o of
    Just (String ref) ->
      case resolveRef defs ref of
        Just resolved -> parseFieldSchema defs resolved
        Nothing -> pure $ FRef ref
    _ -> parseFieldType defs o

parseFieldSchema _ (String s) =
  -- Sometimes type is just a string
  pure $ parseSimpleType s

parseFieldSchema _ _ = pure FAny

-- | Parse field type from object
parseFieldType :: Map Text Value -> Object -> Either Text FieldSchema
parseFieldType defs o =
  case KM.lookup "const" o of
    Just (String c) -> pure $ FConst c
    _ -> case KM.lookup "type" o of
      Just (String t) -> parseTypedField defs o t
      Just (Array ts) -> parseNullableType defs o ts
      _ ->
        -- Check for anyOf (another nullable pattern)
        case KM.lookup "anyOf" o of
          Just (Array opts) -> parseAnyOf defs opts
          _ -> pure FAny

-- | Parse a typed field
parseTypedField :: Map Text Value -> Object -> Text -> Either Text FieldSchema
parseTypedField defs o typ = case typ of
  "string"  -> pure FString
  "integer" -> pure FInteger
  "number"  -> pure FNumber
  "boolean" -> pure FBoolean
  "array"   -> do
    items <- case KM.lookup "items" o of
      Just v  -> parseFieldSchema defs v
      Nothing -> pure FAny
    pure $ FArray items
  "object"  -> do
    let required = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- V.toList arr]
          _ -> []
    fields <- case KM.lookup "properties" o of
      Just (Object props) -> parseProperties defs required props
      _ -> pure []
    pure $ FObject fields
  "null"    -> pure FAny
  _         -> pure FAny

-- | Parse nullable type: ["string", "null"]
parseNullableType :: Map Text Value -> Object -> V.Vector Value -> Either Text FieldSchema
parseNullableType defs o types = do
  let nonNull = filter (/= String "null") (V.toList types)
  case nonNull of
    [String t] -> do
      inner <- parseTypedField defs o t
      pure $ FNullable inner
    _ -> pure FAny

-- | Parse anyOf (another nullable pattern)
parseAnyOf :: Map Text Value -> V.Vector Value -> Either Text FieldSchema
parseAnyOf defs opts = do
  let nonNull = filter (not . isNullType) (V.toList opts)
  case nonNull of
    [v] -> do
      inner <- parseFieldSchema defs v
      pure $ FNullable inner
    _ -> pure FAny
  where
    isNullType (Object o) = KM.lookup "type" o == Just (String "null")
    isNullType _ = False

-- | Parse simple type string
parseSimpleType :: Text -> FieldSchema
parseSimpleType = \case
  "string"  -> FString
  "integer" -> FInteger
  "number"  -> FNumber
  "boolean" -> FBoolean
  _         -> FAny

-- | Resolve a $ref to its definition
resolveRef :: Map Text Value -> Text -> Maybe Value
resolveRef defs ref
  | "#/$defs/" `T.isPrefixOf` ref = Map.lookup (T.drop 8 ref) defs
  | otherwise = Nothing

-- | Extract discriminator value from fields (looks for FConst in "type" field)
extractDiscriminatorValue :: ObjectFields -> Maybe Text
extractDiscriminatorValue fields =
  case lookup "type" [(n, s) | (n, s, _) <- fields] of
    Just (FConst v) -> Just v
    _ -> Nothing

-- | Find common discriminator field name across variants
findDiscriminator :: [VariantSchema] -> Maybe Text
findDiscriminator variants =
  -- All variants have a "type" field with const, so "type" is the discriminator
  if all (hasConstType . variantFields) variants
    then Just "type"
    else Nothing
  where
    hasConstType fields = any isTypeConst fields
    isTypeConst ("type", FConst _, _) = True
    isTypeConst _ = False

-- | Convert Aeson Key to Text
keyToText :: Key -> Text
keyToText = K.toText

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Get a human-readable type name for a field schema
fieldSchemaType :: FieldSchema -> Text
fieldSchemaType = \case
  FString       -> "string"
  FInteger      -> "integer"
  FNumber       -> "number"
  FBoolean      -> "boolean"
  FArray inner  -> "array<" <> fieldSchemaType inner <> ">"
  FObject _     -> "object"
  FNullable inner -> fieldSchemaType inner <> "?"
  FRef ref      -> "ref:" <> ref
  FConst c      -> "const:" <> c
  FAny          -> "any"

-- | Check if a variant represents streaming content (has 'content' field)
isStreamingVariant :: VariantSchema -> Bool
isStreamingVariant v = any isContentField (variantFields v)
  where
    isContentField ("content", FString, _) = True
    isContentField _ = False

-- | Get the discriminator field value for a variant
getDiscriminator :: VariantSchema -> Text
getDiscriminator = variantName
