{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IR Builder - walks schema tree and constructs the IR
--
-- Uses the existing schema walker with a custom algebra that:
-- 1. Extracts types from $defs in methodParams and methodReturns
-- 2. Deduplicates types by name
-- 3. Infers streaming from return type structure
-- 4. Builds method definitions with type references
module Synapse.IR.Builder
  ( -- * Building IR
    buildIR

    -- * Extraction (for testing)
  , extractTypesFromSchema
  , extractMethodDef
  , schemaToTypeRef
  ) where

import Control.Monad (forM)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Synapse.Schema.Types
import Synapse.Schema.Functor (SchemaF(..))
import Synapse.Algebra.Walk (walkSchema)
import Synapse.Monad
import Synapse.IR.Types

-- ============================================================================
-- Building IR
-- ============================================================================

-- | Build IR by walking the schema tree from a given path
buildIR :: Path -> SynapseM IR
buildIR = walkSchema irAlgebra

-- | Algebra for building IR from schema tree
--
-- At each node:
-- - Extract types from all methods' params and returns
-- - Build method definitions
-- - Merge with child results
irAlgebra :: SchemaF IR -> SynapseM IR
irAlgebra (PluginF schema path childIRs) = do
  let namespace = psNamespace schema
      pathPrefix = T.intercalate "." path

  -- Extract types and methods from this plugin
  let (localTypes, localMethods) = extractFromPlugin namespace pathPrefix schema

  -- Merge with children
  let childIR = foldr mergeIR emptyIR childIRs
  let pluginMethods = map mdName (Map.elems localMethods)

  -- Use this plugin's hash if at root (path is empty or just namespace)
  let thisHash = if null path || path == [namespace]
                 then Just (psHash schema)
                 else irHash childIR

  pure $ IR
    { irVersion = "1.0"
    , irHash = thisHash
    , irTypes = Map.union localTypes (irTypes childIR)  -- Local wins on conflict
    , irMethods = Map.union localMethods (irMethods childIR)
    , irPlugins = Map.insert namespace pluginMethods (irPlugins childIR)
    }

irAlgebra (MethodF method namespace path) = do
  -- Single method node (shouldn't happen in normal walk, but handle it)
  let fullPath = T.intercalate "." path
  let (types, mdef) = extractMethodDef namespace fullPath method
  pure $ IR
    { irVersion = "1.0"
    , irHash = Nothing  -- Methods don't carry hash
    , irTypes = types
    , irMethods = Map.singleton fullPath mdef
    , irPlugins = Map.singleton namespace [methodName method]
    }

-- ============================================================================
-- Extraction from Plugin
-- ============================================================================

-- | Extract all types and methods from a plugin schema
-- Types are namespace-qualified to avoid collisions (e.g., "cone.ListResult")
extractFromPlugin :: Text -> Text -> PluginSchema -> (Map Text TypeDef, Map Text MethodDef)
extractFromPlugin namespace pathPrefix schema =
  let methods = psMethods schema
      results = map (extractMethodDef namespace pathPrefix) methods
      allTypes = Map.unions (map fst results)
      allMethods = Map.fromList
        [ (mdFullPath m, m)
        | (_, m) <- results
        ]
  in (allTypes, allMethods)

-- | Extract types and method def from a single method
extractMethodDef :: Text -> Text -> MethodSchema -> (Map Text TypeDef, MethodDef)
extractMethodDef namespace pathPrefix method =
  let name = methodName method
      fullPath = if T.null pathPrefix
                 then namespace <> "." <> name
                 else pathPrefix <> "." <> name

      -- Extract types from params (namespace-qualified)
      (paramTypes, params) = extractParams namespace (methodParams method)

      -- Extract types from returns (namespace-qualified)
      (returnTypes, returnRef, streaming) = extractReturns namespace name (methodReturns method)

      -- Combine all types
      allTypes = Map.union paramTypes returnTypes

      mdef = MethodDef
        { mdName = name
        , mdFullPath = fullPath
        , mdNamespace = namespace
        , mdDescription = Just (methodDescription method)
        , mdStreaming = streaming
        , mdParams = params
        , mdReturns = returnRef
        }
  in (allTypes, mdef)

-- ============================================================================
-- Parameter Extraction
-- ============================================================================

-- | Extract types and param defs from method params schema
-- Types are namespace-qualified to avoid collisions
extractParams :: Text -> Maybe Value -> (Map Text TypeDef, [ParamDef])
extractParams _ Nothing = (Map.empty, [])
extractParams namespace (Just val) = case val of
  Object o -> extractParamsFromObject namespace o
  _ -> (Map.empty, [])

extractParamsFromObject :: Text -> KM.KeyMap Value -> (Map Text TypeDef, [ParamDef])
extractParamsFromObject namespace o =
  let -- Extract $defs (namespace-qualified)
      defs = extractDefs namespace o

      -- Extract properties
      props = case KM.lookup "properties" o of
        Just (Object p) -> KM.toList p
        _ -> []

      -- Get required list
      required = case KM.lookup "required" o of
        Just (Array arr) -> [t | String t <- V.toList arr]
        _ -> []

      -- Build param defs
      params =
        [ ParamDef
            { pdName = K.toText k
            , pdType = schemaToTypeRef namespace v
            , pdDescription = extractDescription v
            , pdRequired = K.toText k `elem` required
            , pdDefault = extractDefault v
            }
        | (k, v) <- props
        ]
  in (defs, params)

-- ============================================================================
-- Return Type Extraction
-- ============================================================================

-- | Extract types, return ref, and streaming flag from returns schema
extractReturns :: Text -> Text -> Maybe Value -> (Map Text TypeDef, TypeRef, Bool)
extractReturns _ _ Nothing = (Map.empty, RefUnknown, False)
extractReturns namespace methodName (Just val) = case val of
  Object o ->
    let -- Extract $defs
        defs = extractDefs namespace o

        -- Get the type name from title or generate from method name
        typeName = case KM.lookup "title" o of
          Just (String t) -> t
          _ -> methodName <> "Result"

        -- Check for oneOf (discriminated union)
        (typeDef, streaming) = case KM.lookup "oneOf" o of
          Just (Array variants) ->
            let variantDefs = mapMaybe (extractVariant namespace) (V.toList variants)
                discriminator = inferDiscriminator variantDefs
                nonErrorVariants = filter (\v -> vdName v /= "error") variantDefs
                isStream = length nonErrorVariants > 1
            in ( Just $ TypeDef
                   { tdName = typeName
                   , tdNamespace = namespace
                   , tdDescription = extractDescription val
                   , tdKind = KindEnum discriminator variantDefs
                   }
               , isStream
               )
          _ ->
            -- Not a union, just a regular type
            (Nothing, False)

        -- Add the return type to defs if it's a union
        allDefs = case typeDef of
          Just td -> Map.insert (tdFullName td) td defs
          Nothing -> defs

        -- Return type reference uses full name for lookup
        fullTypeName = namespace <> "." <> typeName

    in (allDefs, RefNamed fullTypeName, streaming)
  _ -> (Map.empty, RefUnknown, False)

-- | Extract a variant from a oneOf element
extractVariant :: Text -> Value -> Maybe VariantDef
extractVariant namespace (Object o) = case KM.lookup "properties" o of
  Just (Object props) ->
    -- Find the discriminator value (look for "type" with const)
    let discriminatorValue = case KM.lookup "type" props of
          Just (Object typeObj) -> case KM.lookup "const" typeObj of
            Just (String s) -> Just s
            _ -> Nothing
          _ -> Nothing

        -- Extract fields (excluding the discriminator)
        fields =
          [ FieldDef
              { fdName = K.toText k
              , fdType = schemaToTypeRef namespace v
              , fdDescription = extractDescription v
              , fdRequired = True  -- In variants, fields are typically required
              , fdDefault = Nothing
              }
          | (k, v) <- KM.toList props
          , K.toText k /= "type"  -- Exclude discriminator
          ]
    in case discriminatorValue of
         Just dv -> Just $ VariantDef
           { vdName = dv
           , vdDescription = extractDescription (Object o)
           , vdFields = fields
           }
         Nothing -> Nothing
  _ -> Nothing
extractVariant _ _ = Nothing

-- | Infer the discriminator field name from variants
inferDiscriminator :: [VariantDef] -> Text
inferDiscriminator _ = "type"  -- Convention: always "type"

-- | Extract const value from a simple string variant
-- Matches schema like: { "const": "pending", "type": "string", "description": "..." }
extractStringConst :: Value -> Maybe Text
extractStringConst (Object o) =
  case (KM.lookup "const" o, KM.lookup "type" o) of
    (Just (String c), Just (String "string")) -> Just c
    _ -> Nothing
extractStringConst _ = Nothing

-- ============================================================================
-- Type Extraction from $defs
-- ============================================================================

-- | Extract type definitions from $defs or definitions (draft-07 compatibility)
-- Types are namespace-qualified to avoid collisions
extractDefs :: Text -> KM.KeyMap Value -> Map Text TypeDef
extractDefs namespace o =
  let defs = case KM.lookup "$defs" o of
        Just (Object d) -> d
        _ -> case KM.lookup "definitions" o of
          Just (Object d) -> d
          _ -> KM.empty
  in Map.fromList $ mapMaybe (extractTypeDef namespace) (KM.toList defs)

-- | Extract a type definition from a $defs entry
extractTypeDef :: Text -> (K.Key, Value) -> Maybe (Text, TypeDef)
extractTypeDef namespace (k, v) = case v of
  Object o ->
    let name = K.toText k
        desc = extractDescription v
        kind = inferTypeKind namespace o
        td = TypeDef name namespace desc kind
    in Just (tdFullName td, td)
  _ -> Nothing

-- | Infer the kind of a type from its JSON Schema
inferTypeKind :: Text -> KM.KeyMap Value -> TypeKind
inferTypeKind namespace o
  -- Check for oneOf (enum)
  | Just (Array variants) <- KM.lookup "oneOf" o =
      -- First check if this is a simple string enum (all variants are {const: X, type: "string"})
      let maybeStringValues = mapMaybe extractStringConst (V.toList variants)
      in if length maybeStringValues == V.length variants && not (null maybeStringValues)
         then KindStringEnum maybeStringValues
         else let variantDefs = mapMaybe (extractVariant namespace) (V.toList variants)
              in KindEnum "type" variantDefs

  -- Check for enum (simple string enum)
  | Just (Array values) <- KM.lookup "enum" o =
      let stringValues = [ asText v | v <- V.toList values ]
      in KindStringEnum stringValues

  -- Check for object with properties (struct)
  | Just (Object props) <- KM.lookup "properties" o =
      let required = case KM.lookup "required" o of
            Just (Array arr) -> [t | String t <- V.toList arr]
            _ -> []
          fields =
            [ FieldDef
                { fdName = K.toText k
                , fdType = schemaToTypeRef namespace v
                , fdDescription = extractDescription v
                , fdRequired = K.toText k `elem` required
                , fdDefault = extractDefault v
                }
            | (k, v) <- KM.toList props
            ]
      in KindStruct fields

  -- Check for primitive types
  | Just typeVal <- KM.lookup "type" o =
      case typeVal of
        String t -> KindPrimitive t (extractFormat o)
        Array ts ->
          -- Nullable type like ["string", "null"]
          let nonNull = [t | String t <- V.toList ts, t /= "null"]
          in case nonNull of
               [t] -> KindPrimitive t (extractFormat o)
               _ -> KindPrimitive "any" Nothing
        _ -> KindPrimitive "any" Nothing

  -- Default to unknown
  | otherwise = KindPrimitive "any" Nothing

  where
    asText (String s) = s
    asText _ = "unknown"

-- ============================================================================
-- Schema to TypeRef Conversion
-- ============================================================================

-- | Convert a JSON Schema value to a TypeRef
--
-- Distinguishes between:
-- - RefAny: Schema present but no type constraints (intentionally dynamic, e.g. serde_json::Value)
-- - RefUnknown: No schema at all (schema gap, should warn)
--
-- Type references via $ref are namespace-qualified (e.g., "cone.ConeInfo")
schemaToTypeRef :: Text -> Value -> TypeRef
schemaToTypeRef namespace (Object o)
  -- Check for $ref - namespace-qualify the reference
  | Just (String ref) <- KM.lookup "$ref" o =
      RefNamed (namespace <> "." <> extractRefName ref)

  -- Check for array
  | Just (String "array") <- KM.lookup "type" o =
      case KM.lookup "items" o of
        Just items -> RefArray (schemaToTypeRef namespace items)
        Nothing -> RefArray RefAny  -- array without items = any[]

  -- Check for nullable
  | Just (Array types) <- KM.lookup "type" o =
      let nonNull = [t | t@(String s) <- V.toList types, s /= "null"]
          hasNull = any (\case String "null" -> True; _ -> False) (V.toList types)
      in case nonNull of
           [String t] ->
             let base = RefPrimitive t (extractFormat o)
             in if hasNull then RefOptional base else base
           _ -> RefAny  -- Multiple non-null types = any

  -- Check for anyOf (often used for optional refs) - namespace-qualify refs
  | Just (Array options) <- KM.lookup "anyOf" o =
      let refs = mapMaybe extractRefFromOption (V.toList options)
      in case refs of
           [r] -> RefOptional (RefNamed (namespace <> "." <> r))
           _ -> RefAny  -- Complex anyOf = any

  -- Check for primitive type
  | Just (String t) <- KM.lookup "type" o =
      RefPrimitive t (extractFormat o)

  -- Schema object present but no type constraint = intentionally dynamic (RefAny)
  -- This happens with serde_json::Value which emits {"description": "...", "default": null}
  | otherwise = RefAny

-- JSON Schema `true` is the "accept anything" schema - intentionally dynamic
-- This is used when schemars emits a field like `input: true` for serde_json::Value
schemaToTypeRef _ (Bool True) = RefAny

-- Null, false, or non-JSON-Schema values = schema gap (should warn)
schemaToTypeRef _ _ = RefUnknown

-- | Extract ref name from a $ref string like "#/$defs/Position"
extractRefName :: Text -> Text
extractRefName ref = case T.splitOn "/" ref of
  parts | not (null parts) -> last parts
  _ -> ref

-- | Extract ref from anyOf option (for optional types)
extractRefFromOption :: Value -> Maybe Text
extractRefFromOption (Object o) = case KM.lookup "$ref" o of
  Just (String ref) -> Just (extractRefName ref)
  _ -> Nothing
extractRefFromOption _ = Nothing

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract description from a schema
extractDescription :: Value -> Maybe Text
extractDescription (Object o) = case KM.lookup "description" o of
  Just (String s) -> Just s
  _ -> Nothing
extractDescription _ = Nothing

-- | Extract default value from a schema
extractDefault :: Value -> Maybe Value
extractDefault (Object o) = KM.lookup "default" o
extractDefault _ = Nothing

-- | Extract format from a schema object
extractFormat :: KM.KeyMap Value -> Maybe Text
extractFormat o = case KM.lookup "format" o of
  Just (String s) -> Just s
  _ -> Nothing

-- | Extract types from a full schema (for standalone use)
-- Types are namespace-qualified
extractTypesFromSchema :: Text -> Value -> Map Text TypeDef
extractTypesFromSchema namespace (Object o) = extractDefs namespace o
extractTypesFromSchema _ _ = Map.empty
