{-# LANGUAGE ApplicativeDo #-}

-- | Dynamic CLI parser generation from schema
--
-- Builds optparse-applicative parsers at runtime from PlexusSchema.
-- When enriched schemas are available, generates typed flags per parameter.
module Plexus.Dynamic
  ( -- * Types
    CommandInvocation(..)
    -- * Parser Building
  , buildDynamicParser
  , buildDynamicParserWithSchemas
  , buildActivationParser
  , buildMethodParser
  , buildTypedMethodParser
  ) where

import Data.Aeson (Value(..), toJSON, eitherDecode, object, (.=))
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Plexus.Schema (PlexusSchema(..), ActivationInfo(..), EnrichedSchema, ActivationFullSchema(..), MethodSchemaInfo(..), MethodSchema(..), ParamSchema(..), ParamType(..), parseMethodVariantByIndex)

-- ============================================================================
-- Types
-- ============================================================================

-- | Result of parsing a dynamic command
data CommandInvocation = CommandInvocation
  { invMethod :: Text   -- ^ Full method name (e.g., "arbor_tree_create")
  , invParams :: Value  -- ^ JSON parameters (array or object)
  }
  deriving stock (Show, Eq)

-- ============================================================================
-- Parser Building
-- ============================================================================

-- | Build a complete parser from PlexusSchema (without enriched schemas)
--
-- Creates a subparser with one subcommand per activation, each with
-- sub-subcommands for methods. Uses generic --params JSON for all methods.
buildDynamicParser :: PlexusSchema -> Parser CommandInvocation
buildDynamicParser schema = buildDynamicParserWithSchemas schema Map.empty

-- | Build a parser with full schemas for typed parameters
--
-- When an activation has a full schema, generates typed flags
-- like --owner-id TEXT instead of generic --params JSON.
buildDynamicParserWithSchemas :: PlexusSchema -> Map Text ActivationFullSchema -> Parser CommandInvocation
buildDynamicParserWithSchemas schema fullSchemas = subparser $ mconcat
  [ command (T.unpack ns)
      (info (buildActivationParserWithFullSchema act mFullSchema <**> helper)
            (progDesc (T.unpack $ activationDescription act)))
  | act <- sortOn activationNamespace (schemaActivations schema)
  , let ns = activationNamespace act
  , let mFullSchema = Map.lookup ns fullSchemas
  ]

-- | Build a parser for a single activation
buildActivationParser :: ActivationInfo -> Parser CommandInvocation
buildActivationParser act = buildActivationParserWithSchema act Nothing

-- | Build a parser for an activation with optional enriched schema
buildActivationParserWithSchema :: ActivationInfo -> Maybe EnrichedSchema -> Parser CommandInvocation
buildActivationParserWithSchema act mEnriched = subparser $ mconcat
  [ command (toCommandName method)
      (info (methodParser <**> helper)
            (progDesc desc))
  | (idx, method) <- zip [0..] (activationMethods act)
  -- Use index to look up method schema (methods list order matches oneOf order)
  , let mMethodSchema = mEnriched >>= \e -> parseMethodVariantByIndex e idx
        methodParser = case mMethodSchema of
          Just ms -> buildTypedMethodParser ns method ms
          Nothing -> buildMethodParser ns method
        defaultDesc = "Execute " <> T.unpack ns <> "_" <> T.unpack method
        desc = case mMethodSchema of
          Just ms -> maybe defaultDesc T.unpack (methodDescription ms)
          Nothing -> defaultDesc
  ]
  where
    ns = activationNamespace act

-- | Build a parser for an activation with optional full schema
buildActivationParserWithFullSchema :: ActivationInfo -> Maybe ActivationFullSchema -> Parser CommandInvocation
buildActivationParserWithFullSchema act mFullSchema = subparser $ mconcat
  [ command (toCommandName method)
      (info (methodParser <**> helper)
            (progDesc desc))
  | method <- activationMethods act
  -- Look up method info from full schema by name
  , let mMethodInfo = mFullSchema >>= \fs -> find (\m -> methodInfoName m == method) (fullSchemaMethods fs)
        -- Check if we have params schema to generate typed flags
        methodParser = case mMethodInfo >>= methodInfoParams of
          Just paramsSchema -> buildTypedMethodParserFromParams ns method paramsSchema
          Nothing -> buildMethodParser ns method
        defaultDesc = "Execute " <> T.unpack ns <> "_" <> T.unpack method
        desc = maybe defaultDesc T.unpack (mMethodInfo >>= Just . methodInfoDescription)
  ]
  where
    ns = activationNamespace act
    find f = foldr (\x acc -> if f x then Just x else acc) Nothing

-- | Build a typed method parser from params JSON Schema
buildTypedMethodParserFromParams :: Text -> Text -> Value -> Parser CommandInvocation
buildTypedMethodParserFromParams namespace method paramsSchema = do
  -- Extract properties and required fields from JSON Schema
  let (properties, required) = extractSchemaInfo paramsSchema

  -- Sort parameters: required first, then optional
  let sortedParams = sortOn (\(name, _) -> name `notElem` required) (Map.toList properties)

  -- Build parser for each parameter
  paramValues <- sequenceA $ map (buildParamParserFromSchema required) sortedParams

  -- Also allow raw JSON override
  mJsonOverride <- optional $ strOption
    ( long "params"
   <> short 'p'
   <> metavar "JSON"
   <> help "Override with raw JSON object"
   <> hidden  -- Hide from help since we have typed flags
    )

  pure $ CommandInvocation
    { invMethod = namespace <> "_" <> method
    , invParams = case mJsonOverride of
        Just jsonStr -> case decodeJsonArgs jsonStr of
          Just val -> val
          Nothing -> buildParamsObject paramValues
        Nothing -> buildParamsObject paramValues
    }

-- | Extract properties and required fields from JSON Schema
extractSchemaInfo :: Value -> (Map Text Value, [Text])
extractSchemaInfo (Object o) =
  let properties = case KM.lookup "properties" o of
        Just (Object props) -> Map.fromList [(toText k, v) | (k, v) <- KM.toList props]
        _ -> Map.empty
      required = case KM.lookup "required" o of
        Just (Array arr) -> [t | String t <- foldr (:) [] arr]
        _ -> []
  in (properties, required)
extractSchemaInfo _ = (Map.empty, [])

-- | Build a parser for a single parameter from JSON Schema
buildParamParserFromSchema :: [Text] -> (Text, Value) -> Parser (Text, Maybe Value)
buildParamParserFromSchema required (name, schema) = do
  mVal <- paramParser
  pure (name, mVal)
  where
    nameStr = T.unpack name
    flagName = toFlagName name
    isRequired = name `elem` required

    -- Extract type and description from schema
    (typeStr, desc) = case schema of
      Object o ->
        let t = case KM.lookup "type" o of
              Just (String s) -> s
              _ -> "string"
            d = case KM.lookup "description" o of
              Just (String s) -> Just s
              _ -> Nothing
        in (t, d)
      _ -> ("string", Nothing)

    metaVar = case typeStr of
      "string" -> "TEXT"
      "integer" -> "INT"
      "number" -> "NUM"
      "boolean" -> "BOOL"
      _ -> "VALUE"

    helpText = maybe ("Parameter: " <> nameStr) T.unpack desc

    paramParser :: Parser (Maybe Value)
    paramParser
      | isRequired = Just <$> requiredParser
      | otherwise = optional optionalParser

    requiredParser :: Parser Value
    requiredParser = case typeStr of
      "integer" -> toJSON <$> (option auto
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser Int)
      "number" -> toJSON <$> (option auto
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser Double)
      "boolean" -> toJSON <$> switch
        ( long flagName
       <> help helpText
        )
      _ -> toJSON <$> (strOption
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser String)

    optionalParser :: Parser Value
    optionalParser = case typeStr of
      "integer" -> toJSON <$> (option auto
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser Int)
      "number" -> toJSON <$> (option auto
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser Double)
      "boolean" -> toJSON <$> switch
        ( long flagName
       <> help helpText
        )
      _ -> toJSON <$> (strOption
        ( long flagName
       <> metavar metaVar
       <> help helpText
        ) :: Parser String)

-- | Build a parser for a method with generic --params JSON
buildMethodParser :: Text -> Text -> Parser CommandInvocation
buildMethodParser namespace method = do
  -- Option 1: Raw JSON params
  mJsonParams <- optional $ strOption
    ( long "params"
   <> short 'p'
   <> metavar "JSON"
   <> help "JSON object of parameters"
    )
  -- Option 2: Positional arguments (converted to JSON array)
  posArgs <- many $ strArgument (metavar "ARG...") :: Parser [String]

  pure $ CommandInvocation
    { invMethod = namespace <> "_" <> method
    , invParams = case mJsonParams of
        Just jsonStr ->
          case decodeJsonArgs jsonStr of
            Just val -> val
            Nothing  -> toJSON [jsonStr :: String]
        Nothing ->
          toJSON (posArgs :: [String])
    }

-- | Build a parser for a method with typed parameters from MethodSchema
buildTypedMethodParser :: Text -> Text -> MethodSchema -> Parser CommandInvocation
buildTypedMethodParser namespace method schema = do
  -- Parse each parameter as a typed flag (required params first)
  let sortedParams = sortOn (not . paramRequired) (methodParams schema)
  paramValues <- sequenceA $ map buildParamParser sortedParams

  -- Also allow raw JSON override
  mJsonOverride <- optional $ strOption
    ( long "params"
   <> short 'p'
   <> metavar "JSON"
   <> help "Override with raw JSON object"
   <> hidden  -- Hide from help since we have typed flags
    )

  pure $ CommandInvocation
    { invMethod = namespace <> "_" <> method
    , invParams = case mJsonOverride of
        Just jsonStr -> case decodeJsonArgs jsonStr of
          Just val -> val
          Nothing -> buildParamsObject paramValues
        Nothing -> buildParamsObject paramValues
    }

-- | Build a parser for a single parameter
buildParamParser :: ParamSchema -> Parser (Text, Maybe Value)
buildParamParser param = do
  mVal <- paramParser
  pure (paramName param, mVal)
  where
    name = T.unpack $ paramName param
    flagName = toFlagName (paramName param)
    metaVar = paramMetavar param
    helpText = maybe ("Parameter: " <> name) T.unpack (paramDesc param)

    paramParser :: Parser (Maybe Value)
    paramParser
      | paramRequired param = Just <$> requiredParser
      | otherwise = optional optionalParser

    requiredParser :: Parser Value
    requiredParser = case paramType param of
      ParamBoolean -> toJSON <$> switch
        ( long flagName <> help helpText )
      ParamInteger -> toJSON <$> (option auto
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser Integer)
      ParamNumber -> toJSON <$> (option auto
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser Double)
      _ -> toJSON <$> (strOption
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser String)

    optionalParser :: Parser Value
    optionalParser = case paramType param of
      ParamBoolean -> toJSON <$> switch
        ( long flagName <> help helpText )
      ParamInteger -> toJSON <$> (option auto
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser Integer)
      ParamNumber -> toJSON <$> (option auto
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser Double)
      _ -> toJSON <$> (strOption
        ( long flagName <> metavar metaVar <> help helpText ) :: Parser String)

-- | Build JSON object from parsed parameter values
buildParamsObject :: [(Text, Maybe Value)] -> Value
buildParamsObject params = Object $ KM.fromList
  [ (fromText key, val)
  | (key, Just val) <- params
  ]

-- | Convert parameter name to flag name (snake_case to kebab-case)
toFlagName :: Text -> String
toFlagName = T.unpack . T.replace "_" "-"

-- | Get metavar for a parameter based on its type
paramMetavar :: ParamSchema -> String
paramMetavar param = case paramFormat param of
  Just "uuid" -> "UUID"
  Just fmt -> T.unpack $ T.toUpper fmt
  Nothing -> case paramType param of
    ParamString -> "TEXT"
    ParamInteger -> "INT"
    ParamNumber -> "NUM"
    ParamBoolean -> ""
    ParamObject -> "JSON"
    ParamArray _ -> "JSON"
    ParamNullable t -> paramMetavar param { paramType = t }

-- | Convert method name to command name (snake_case to kebab-case)
toCommandName :: Text -> String
toCommandName = T.unpack . T.replace "_" "-"

-- | Try to decode JSON args
decodeJsonArgs :: String -> Maybe Value
decodeJsonArgs s = case eitherDecode (LBS.pack s) of
  Right v -> Just v
  Left _  -> Nothing
