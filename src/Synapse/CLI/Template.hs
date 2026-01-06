{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | IR-driven Mustache template generation
--
-- Generates structured Mustache templates directly from the IR, providing:
--
-- - Discriminated union handling with variant sections
-- - Structured output with labeled fields
-- - Better formatting than flat JSON Schema parsing
--
-- = Design
--
-- Instead of parsing JSON Schema ad-hoc (like Synapse.Algebra.TemplateGen),
-- we use the pre-built IR which has already resolved types:
--
-- @
-- IR (types, methods)
--   |
--   +-> generateTemplate     -> template for a single method
--   +-> generateAllTemplates -> all templates for a path
--   +-> typeRefToMustache    -> TypeRef -> Mustache text
--   +-> typeDefToMustache    -> TypeDef -> Mustache text
-- @
--
-- = Example Output
--
-- For `cone.chat` (streaming enum with variants):
--
-- @
-- {{! cone.chat }}
-- {{#context}}
-- tree_id: {{tree_id}}
-- node_id: {{node_id}}
-- {{/context}}
-- {{#delta}}
-- {{content}}{{/delta}}
-- {{#done}}
-- tree_id: {{tree_id}}
-- node_id: {{node_id}}
-- {{/done}}
-- {{#error}}
-- Error: {{message}}
-- {{/error}}
-- @
module Synapse.CLI.Template
  ( -- * Template Generation
    generateTemplate
  , generateAllTemplates
  , generateAllTemplatesWithCallback

    -- * Type Conversion
  , typeRefToMustache
  , typeDefToMustache

    -- * Types
  , GeneratedTemplate(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.Monad (SynapseM)
import Synapse.Schema.Types (Path)

-- ============================================================================
-- Types
-- ============================================================================

-- | A generated template with metadata
data GeneratedTemplate = GeneratedTemplate
  { gtNamespace :: Text      -- ^ Plugin namespace (e.g., "cone")
  , gtMethod    :: Text      -- ^ Method name (e.g., "chat")
  , gtTemplate  :: Text      -- ^ Mustache template content
  , gtPath      :: FilePath  -- ^ Relative path for template file
  }
  deriving stock (Show, Eq)

-- ============================================================================
-- High-Level Generation
-- ============================================================================

-- | Generate template for a method's return type
generateTemplate :: IR -> MethodDef -> GeneratedTemplate
generateTemplate ir method =
  let body = typeRefToMustache ir 0 (mdReturns method)
      header = "{{! " <> mdFullPath method <> " }}"
      template = if T.null body || body == "{{.}}"
                 then header <> "\n{{.}}"  -- Fallback for simple types
                 else header <> "\n" <> body
  in GeneratedTemplate
    { gtNamespace = mdNamespace method
    , gtMethod = mdName method
    , gtTemplate = template
    , gtPath = T.unpack (mdNamespace method) </> T.unpack (mdName method) <.> "mustache"
    }

-- | Generate all templates by building IR and iterating methods
generateAllTemplates :: Path -> SynapseM [GeneratedTemplate]
generateAllTemplates path = do
  ir <- buildIR path
  let methods = Map.elems (irMethods ir)
      templates = map (generateTemplate ir) methods
      -- Filter out trivial templates that just render {{.}}
      nonTrivial = filter (not . isTrivialTemplate) templates
  pure nonTrivial

-- | Generate templates with callback for streaming output
generateAllTemplatesWithCallback
  :: (GeneratedTemplate -> IO ())  -- ^ Called for each template
  -> Path
  -> SynapseM Int                  -- ^ Returns count
generateAllTemplatesWithCallback callback path = do
  templates <- generateAllTemplates path
  liftIO $ mapM_ callback templates
  pure $ length templates

-- | Check if a template is trivial (just {{.}})
isTrivialTemplate :: GeneratedTemplate -> Bool
isTrivialTemplate gt =
  let body = T.drop 1 $ T.dropWhile (/= '\n') (gtTemplate gt)
  in T.strip body == "{{.}}"

-- ============================================================================
-- Type Reference to Mustache
-- ============================================================================

-- | Convert a TypeRef to Mustache template text
--
-- This is the core conversion function that handles:
-- - Primitives: {{.}}
-- - Named types: look up and expand
-- - Arrays: {{#.}}...{{/.}}
-- - Optionals: same as inner type
typeRefToMustache :: IR -> Int -> TypeRef -> Text
typeRefToMustache ir indent = \case
  -- Primitives render as the current value
  RefPrimitive _ _ -> "{{.}}"

  -- Named types: look up the definition
  RefNamed name -> case Map.lookup name (irTypes ir) of
    Just typeDef -> typeDefToMustache ir indent typeDef
    Nothing -> "{{.}}"  -- Fallback if type not found

  -- Arrays: iterate with section
  RefArray inner ->
    let itemTemplate = typeRefToMustache ir (indent + 1) inner
        ind = indentText indent
    in T.unlines
      [ ind <> "{{#.}}"
      , itemTemplate
      , ind <> "{{/.}}"
      ]

  -- Optionals: just render the inner type (mustache handles missing)
  RefOptional inner -> typeRefToMustache ir indent inner

  -- Any/Unknown: fall back to current value
  RefAny -> "{{.}}"
  RefUnknown -> "{{.}}"

-- ============================================================================
-- Type Definition to Mustache
-- ============================================================================

-- | Convert a TypeDef to Mustache template text
typeDefToMustache :: IR -> Int -> TypeDef -> Text
typeDefToMustache ir indent TypeDef{..} = case tdKind of
  -- Discriminated union: render each variant as a section
  KindEnum discriminator variants ->
    generateUnionTemplate ir indent discriminator variants

  -- Struct: render each field
  KindStruct fields ->
    generateStructTemplate ir indent fields

  -- Primitive: just the value
  KindPrimitive _ _ -> "{{.}}"

  -- Alias: follow to target
  KindAlias target -> typeRefToMustache ir indent target

-- ============================================================================
-- Union Template Generation
-- ============================================================================

-- | Generate template for a discriminated union (oneOf)
--
-- Each variant becomes a mustache section based on its variant name.
-- We use the variant name as the section key since mustache will only
-- render the section if that key is present/truthy in the data.
--
-- Example output:
-- @
-- {{#context}}
-- tree_id: {{tree_id}}
-- {{/context}}
-- {{#delta}}
-- {{content}}{{/delta}}
-- {{#error}}
-- Error: {{message}}
-- {{/error}}
-- @
generateUnionTemplate :: IR -> Int -> Text -> [VariantDef] -> Text
generateUnionTemplate ir indent _discriminator variants =
  T.intercalate "\n" $ map (generateVariantSection ir indent) variants

-- | Generate a section for a single variant
generateVariantSection :: IR -> Int -> VariantDef -> Text
generateVariantSection ir indent VariantDef{..} =
  let ind = indentText indent
      sectionName = vdName
  in case vdFields of
    -- No fields: just render the variant name presence
    [] -> ind <> "{{#" <> sectionName <> "}}{{/" <> sectionName <> "}}"

    -- Single field named "content" or similar: inline rendering
    [field] | isContentField field ->
      ind <> "{{#" <> sectionName <> "}}{{" <> fdName field <> "}}{{/" <> sectionName <> "}}"

    -- Multiple fields: render as labeled lines
    fields ->
      let fieldLines = map (generateFieldLine ir (indent + 1)) fields
          -- Special handling for error variants
          prefix = if vdName == "error"
                   then [ind <> "{{#" <> sectionName <> "}}", indentText (indent + 1) <> "Error:"]
                   else [ind <> "{{#" <> sectionName <> "}}"]
      in T.unlines $ prefix ++ fieldLines ++ [ind <> "{{/" <> sectionName <> "}}"]

-- | Check if a field is a "content" field (for inline rendering)
isContentField :: FieldDef -> Bool
isContentField FieldDef{..} = fdName `elem` ["content", "message", "text", "data"]

-- | Generate a labeled line for a field
generateFieldLine :: IR -> Int -> FieldDef -> Text
generateFieldLine ir indent FieldDef{..} =
  let ind = indentText indent
      label = fdName <> ": "
      value = generateFieldValue ir fdType fdName
  in ind <> label <> value

-- | Generate the value portion of a field
generateFieldValue :: IR -> TypeRef -> Text -> Text
generateFieldValue ir typeRef fieldName = case typeRef of
  -- Primitives: simple interpolation
  RefPrimitive _ _ -> "{{" <> fieldName <> "}}"

  -- Arrays: iterate
  RefArray inner -> case inner of
    RefPrimitive _ _ ->
      "{{#" <> fieldName <> "}}{{.}} {{/" <> fieldName <> "}}"
    _ ->
      "{{#" <> fieldName <> "}}...{{/" <> fieldName <> "}}"

  -- Named types: depends on what they are
  RefNamed name -> case Map.lookup name (irTypes ir) of
    Just TypeDef{tdKind = KindPrimitive _ _} ->
      "{{" <> fieldName <> "}}"
    Just TypeDef{tdKind = KindStruct _} ->
      "{{" <> fieldName <> "}}"  -- Could expand, but often too verbose
    Just TypeDef{tdKind = KindEnum _ _} ->
      "{{" <> fieldName <> "}}"  -- Nested union: keep simple
    _ ->
      "{{" <> fieldName <> "}}"

  -- Optional: same as inner
  RefOptional inner -> generateFieldValue ir inner fieldName

  -- Fallback
  RefAny -> "{{" <> fieldName <> "}}"
  RefUnknown -> "{{" <> fieldName <> "}}"

-- ============================================================================
-- Struct Template Generation
-- ============================================================================

-- | Generate template for a struct
--
-- Renders each field on its own line with a label.
-- Filters out internal fields like "type" (discriminator).
generateStructTemplate :: IR -> Int -> [FieldDef] -> Text
generateStructTemplate ir indent fields =
  let displayFields = filter (not . isInternalField) fields
      fieldLines = map (generateFieldLine ir indent) displayFields
  in T.unlines fieldLines

-- | Check if a field is internal (shouldn't be displayed)
isInternalField :: FieldDef -> Bool
isInternalField FieldDef{..} = fdName `elem` ["type", "event"]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Generate indentation text
indentText :: Int -> Text
indentText n = T.replicate (n * 2) " "
