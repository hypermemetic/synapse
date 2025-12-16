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
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Plexus.Schema (PlexusSchema(..), ActivationInfo(..), EnrichedSchema, MethodSchema(..), ParamSchema(..), ParamType(..), parseMethodVariantByIndex)

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

-- | Build a parser with enriched schemas for typed parameters
--
-- When an activation has an enriched schema, generates typed flags
-- like --owner-id TEXT instead of generic --params JSON.
buildDynamicParserWithSchemas :: PlexusSchema -> Map Text EnrichedSchema -> Parser CommandInvocation
buildDynamicParserWithSchemas schema enriched = subparser $ mconcat
  [ command (T.unpack ns)
      (info (buildActivationParserWithSchema act mEnriched <**> helper)
            (progDesc (T.unpack $ activationDescription act)))
  | act <- sortOn activationNamespace (schemaActivations schema)
  , let ns = activationNamespace act
  , let mEnriched = Map.lookup ns enriched
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
