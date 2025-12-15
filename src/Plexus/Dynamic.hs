{-# LANGUAGE ApplicativeDo #-}

-- | Dynamic CLI parser generation from schema
--
-- Builds optparse-applicative parsers at runtime from PlexusSchema.
module Plexus.Dynamic
  ( -- * Types
    CommandInvocation(..)
    -- * Parser Building
  , buildDynamicParser
  , buildActivationParser
  , buildMethodParser
  ) where

import Data.Aeson (Value(..), toJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Plexus.Schema (PlexusSchema(..), ActivationInfo(..))

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

-- | Build a complete parser from PlexusSchema
--
-- Creates a subparser with one subcommand per activation, each with
-- sub-subcommands for methods.
buildDynamicParser :: PlexusSchema -> Parser CommandInvocation
buildDynamicParser schema = subparser $ mconcat
  [ command (T.unpack ns)
      (info (buildActivationParser act <**> helper)
            (progDesc (T.unpack $ activationDescription act)))
  | act <- sortOn activationNamespace (schemaActivations schema)
  , let ns = activationNamespace act
  ]

-- | Build a parser for a single activation
--
-- Creates subcommands for each method in the activation.
buildActivationParser :: ActivationInfo -> Parser CommandInvocation
buildActivationParser act = subparser $ mconcat
  [ command (toCommandName method)
      (info (buildMethodParser ns method <**> helper)
            (progDesc $ "Execute " <> T.unpack ns <> "_" <> T.unpack method))
  | method <- sortOn id (activationMethods act)
  ]
  where
    ns = activationNamespace act

-- | Build a parser for a single method
--
-- For now, accepts raw JSON arguments since we don't have enriched schemas.
-- Format: method [--params JSON] or method ARG1 ARG2 ...
buildMethodParser :: Text -> Text -> Parser CommandInvocation
buildMethodParser namespace method = do
  -- Option 1: Raw JSON params
  mJsonParams <- optional $ strOption
    ( long "params"
   <> short 'p'
   <> metavar "JSON"
   <> help "JSON array of parameters"
    )
  -- Option 2: Positional arguments (converted to JSON array)
  posArgs <- many $ strArgument (metavar "ARG...") :: Parser [String]

  pure $ CommandInvocation
    { invMethod = namespace <> "_" <> method
    , invParams = case mJsonParams of
        Just jsonStr ->
          -- Try to parse as JSON, fallback to string array
          case decodeJsonArgs jsonStr of
            Just val -> val
            Nothing  -> toJSON [jsonStr :: String]
        Nothing ->
          -- Convert positional args to JSON array
          toJSON (posArgs :: [String])
    }

-- | Convert method name to command name (snake_case to kebab-case)
toCommandName :: Text -> String
toCommandName = T.unpack . T.replace "_" "-"

-- | Try to decode JSON args
decodeJsonArgs :: String -> Maybe Value
decodeJsonArgs s = case eitherDecode (LBS.pack s) of
  Right v -> Just v
  Left _  -> Nothing
