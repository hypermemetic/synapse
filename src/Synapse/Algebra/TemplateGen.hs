-- | Template generation from schemas
--
-- Generates mustache templates from method return schemas.
-- This is a catamorphism over JSON Schema structure.
--
-- = Strategy
--
-- 1. If returns has oneOf/anyOf, generate conditionals for each variant
-- 2. For objects, list all properties
-- 3. For arrays, generate iteration
-- 4. For primitives, just output the value
module Synapse.Algebra.TemplateGen
  ( -- * Generation
    generateTemplate
  , generateTemplateFor
  , generateAllTemplates

    -- * Types
  , GeneratedTemplate(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector as V
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text.IO as TIO

import Synapse.Schema.Types
import Synapse.Monad
import Synapse.Transport (fetchMethodSchema)
import Synapse.Algebra.Walk

-- | A generated template with metadata
data GeneratedTemplate = GeneratedTemplate
  { gtNamespace :: Text      -- ^ Plugin namespace
  , gtMethod    :: Text      -- ^ Method name
  , gtTemplate  :: Text      -- ^ Mustache template content
  , gtPath      :: FilePath  -- ^ Relative path for template file
  }
  deriving stock (Show, Eq)

-- | Generate template for a method
-- Template named by method (matches content_type: namespace.method)
generateTemplate :: MethodInfo -> [GeneratedTemplate]
generateTemplate MethodInfo{..} =
  [ GeneratedTemplate
    { gtNamespace = miNamespace
    , gtMethod = methodName miMethod
    , gtTemplate = "{{! " <> miNamespace <> "." <> methodName miMethod <> " }}\n" <> templateBody
    , gtPath = T.unpack miNamespace </> T.unpack (methodName miMethod) <.> "mustache"
    }
  ]
  where
    templateBody = case methodReturns miMethod of
      Nothing -> "{{.}}"
      Just schema -> schemaToMustache schema

-- | Extract event types from schema (oneOf/anyOf with event discriminator)
extractEventTypes :: Value -> [(Text, Value)]
extractEventTypes (Object o) = case KM.lookup "oneOf" o of
  Just (Array variants) -> mapMaybe extractEvent (V.toList variants)
  Nothing -> case KM.lookup "anyOf" o of
    Just (Array variants) -> mapMaybe extractEvent (V.toList variants)
    Nothing -> []
extractEventTypes _ = []

-- | Extract event name and schema from a variant
extractEvent :: Value -> Maybe (Text, Value)
extractEvent v@(Object o) = case KM.lookup "properties" o of
  Just (Object props) -> case KM.lookup "event" props of
    Just eventSchema -> case extractConstValue eventSchema of
      "event" -> Nothing  -- fallback value, no const found
      evtName -> Just (evtName, v)
    Nothing -> Nothing
  Nothing -> Nothing
extractEvent _ = Nothing

-- | Extract const value from a schema (for discriminators)
extractConstValue :: Value -> Text
extractConstValue (Object o) = case KM.lookup "const" o of
  Just (String s) -> s
  _ -> "event"
extractConstValue _ = "event"

-- | Generate template for a specific method path
generateTemplateFor :: Path -> Text -> SynapseM GeneratedTemplate
generateTemplateFor path methodName' = do
  schema <- fetchMethodSchema path methodName'
  let namespace = if null path then "plexus" else T.intercalate "." path
  pure $ GeneratedTemplate
    { gtNamespace = namespace
    , gtMethod = methodName'
    , gtTemplate = generateFromReturns namespace methodName' (methodReturns schema)
    , gtPath = T.unpack namespace </> T.unpack methodName' <.> "mustache"
    }

-- | Generate all templates by walking the schema tree
generateAllTemplates :: Path -> SynapseM [GeneratedTemplate]
generateAllTemplates path =
  foldMethods generateTemplate concat path

-- | Generate mustache template from a method's return schema
generateFromReturns :: Text -> Text -> Maybe Value -> Text
generateFromReturns namespace method Nothing =
  "{{! " <> namespace <> "." <> method <> " - no return schema }}\n{{.}}"
generateFromReturns namespace method (Just schema) =
  "{{! " <> namespace <> "." <> method <> " }}\n" <> schemaToMustache schema

-- | Convert JSON Schema to mustache template
-- This is the core catamorphism over schema structure
schemaToMustache :: Value -> Text
schemaToMustache (Object o) = case KM.lookup "oneOf" o of
  -- Handle oneOf (discriminated union by 'event' field)
  Just (Array variants) -> generateVariants (V.toList variants)
  Nothing -> case KM.lookup "anyOf" o of
    Just (Array variants) -> generateVariants (V.toList variants)
    Nothing -> case KM.lookup "type" o of
      Just (String "object") -> generateObject o
      Just (String "array") -> generateArray o
      Just (String "string") -> "{{.}}"
      Just (String "number") -> "{{.}}"
      Just (String "integer") -> "{{.}}"
      Just (String "boolean") -> "{{.}}"
      _ -> generateObject o  -- Default to object handling
schemaToMustache _ = "{{.}}"

-- | Generate template for oneOf/anyOf variants
-- Collect all unique properties across variants (flat template, no sections)
generateVariants :: [Value] -> Text
generateVariants variants =
  let allProps = concatMap extractProps variants
      uniqueKeys = dedupe $ map fst allProps
      displayKeys = filter (not . isInternalField) uniqueKeys
  in T.intercalate " " $ map (\k -> "{{" <> k <> "}}") displayKeys
  where
    extractProps :: Value -> [(Text, Value)]
    extractProps (Object o) = case KM.lookup "properties" o of
      Just (Object props) -> [(K.toText k, v) | (k, v) <- KM.toList props]
      Nothing -> []
    extractProps _ = []

    dedupe :: Eq a => [a] -> [a]
    dedupe [] = []
    dedupe (x:xs) = x : dedupe (filter (/= x) xs)

-- | Generate template for object properties
generateObject :: KM.KeyMap Value -> Text
generateObject o = case KM.lookup "properties" o of
  Just (Object props) -> generateProps props
  Nothing -> "{{.}}"

-- | Generate output for properties
generateProps :: KM.KeyMap Value -> Text
generateProps props =
  let keys = map K.toText $ KM.keys props
      -- Filter out internal fields like 'event'
      displayKeys = filter (not . isInternalField) keys
      lines' = map (\k -> "{{" <> k <> "}}") displayKeys
  in T.intercalate " " lines'

-- | Check if a field is internal (shouldn't be displayed)
isInternalField :: Text -> Bool
isInternalField "event" = True
isInternalField "type" = True
isInternalField _ = False

-- | Generate template for array
generateArray :: KM.KeyMap Value -> Text
generateArray o = case KM.lookup "items" o of
  Just itemSchema ->
    "{{#.}}\n" <> schemaToMustache itemSchema <> "\n{{/.}}"
  Nothing -> "{{#.}}{{.}}{{/.}}"

-- | Write generated templates to disk
writeTemplates :: FilePath -> [GeneratedTemplate] -> IO ()
writeTemplates baseDir templates = do
  mapM_ (writeTemplate baseDir) templates

-- | Write a single template to disk
writeTemplate :: FilePath -> GeneratedTemplate -> IO ()
writeTemplate baseDir gt = do
  let fullPath = baseDir </> gtPath gt
      dir = takeDirectory fullPath
  createDirectoryIfMissing True dir
  TIO.writeFile fullPath (gtTemplate gt)
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse
