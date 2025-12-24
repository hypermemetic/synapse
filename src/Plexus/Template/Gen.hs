{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Generate Mustache templates from parsed JSON Schema
--
-- This module provides schema-driven template generation with smart defaults:
--   - Content fields are rendered prominently
--   - IDs and timestamps are formatted appropriately
--   - Arrays are iterated with bullet points
--   - Nested objects recurse naturally
--
-- Two rendering modes are supported:
--   - Streaming: Each variant renders independently as events arrive
--   - Buffered: Events are collected, then rendered as a unified view
module Plexus.Template.Gen
  ( -- * Types
    TemplateSet(..)
  , RenderMode(..)
  , TemplateFragment(..)
    -- * Generation
  , genTemplates
  , genVariantTemplate
  , genFieldTemplate
    -- * Compilation
  , compileFragment
  , compileFragments
    -- * Heuristics
  , fieldPriority
  , isMetaField
  , formatFieldName
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T

import Plexus.Template.Schema

-- ============================================================================
-- Types
-- ============================================================================

-- | How to render stream events
data RenderMode
  = Streaming    -- ^ Render each event as it arrives (default)
  | Buffered     -- ^ Collect events, render unified view at end
  deriving stock (Show, Eq)

-- | Template AST fragment (compiles to Mustache)
data TemplateFragment
  = TLit Text                            -- ^ Literal text
  | TVar Text                            -- ^ Variable: {{{name}}}
  | TEsc Text                            -- ^ Escaped variable: {{name}}
  | TSection Text [TemplateFragment]     -- ^ Section: {{#name}}...{{/name}}
  | TInverted Text [TemplateFragment]    -- ^ Inverted: {{^name}}...{{/name}}
  | TSeq [TemplateFragment]              -- ^ Sequence of fragments
  | TNewline                             -- ^ Newline
  | TIndent Int [TemplateFragment]       -- ^ Indented block
  deriving stock (Show, Eq)

-- | Generated template set for a method
data TemplateSet = TemplateSet
  { tsMode        :: RenderMode
  , tsVariants    :: Map Text Text       -- ^ variant name → compiled mustache
  , tsBuffered    :: Maybe Text          -- ^ For sync mode: combined template
  , tsMethodName  :: Text                -- ^ Method this is for
  } deriving stock (Show, Eq)

-- ============================================================================
-- Generation
-- ============================================================================

-- | Generate templates from a return schema
genTemplates :: Text -> ReturnSchema -> TemplateSet
genTemplates methodName schema = case schema of
  RSPrimitive fs ->
    TemplateSet
      { tsMode = Streaming
      , tsVariants = Map.singleton "data" (compileFragment $ genPrimitiveTemplate fs)
      , tsBuffered = Nothing
      , tsMethodName = methodName
      }

  RSSingle{..} ->
    TemplateSet
      { tsMode = Streaming
      , tsVariants = Map.singleton "data" (compileFragment $ genObjectTemplate rsSingleFields)
      , tsBuffered = Nothing
      , tsMethodName = methodName
      }

  RSUnion{..} ->
    let variantTemplates = Map.fromList
          [ (variantName v, compileFragment $ genVariantTemplate v)
          | v <- rsVariants
          ]
        -- For buffered mode, generate a combined template
        bufferedTmpl = genBufferedTemplate rsVariants
    in TemplateSet
      { tsMode = Streaming  -- Default to streaming
      , tsVariants = variantTemplates
      , tsBuffered = Just (compileFragment bufferedTmpl)
      , tsMethodName = methodName
      }

-- | Generate template for a single variant
genVariantTemplate :: VariantSchema -> TemplateFragment
genVariantTemplate VariantSchema{..} =
  case variantName of
    -- Special case: chat_content - just output content directly for streaming
    "chat_content" ->
      TVar "content"

    -- Special case: error - format as error message
    "error" ->
      TSeq [TLit "Error: ", TVar "message", TNewline]

    -- Special case: list types - format as bullet list
    _ | "_list" `T.isSuffixOf` variantName ->
      genListVariantTemplate variantFields

    -- Special case: completion/done events - minimal output
    _ | any (`T.isSuffixOf` variantName) ["_complete", "_done", "_deleted"] ->
      genCompletionTemplate variantName variantFields

    -- Default: render all fields nicely
    _ -> genObjectTemplate variantFields

-- | Generate template for list-type variants
genListVariantTemplate :: ObjectFields -> TemplateFragment
genListVariantTemplate fields =
  case findListField fields of
    Just (listName, FArray itemSchema) ->
      TSection listName [genListItemTemplate itemSchema, TNewline]
    _ ->
      genObjectTemplate fields

-- | Find the primary list field in a list variant
findListField :: ObjectFields -> Maybe (Text, FieldSchema)
findListField fields =
  case [(n, s) | (n, s@(FArray _), _) <- fields] of
    [(name, schema)] -> Just (name, schema)
    _ -> Nothing

-- | Generate template for list items
genListItemTemplate :: FieldSchema -> TemplateFragment
genListItemTemplate = \case
  FObject objFields ->
    TSeq $ [TLit "  - "] ++ genInlineFields objFields
  FString ->
    TSeq [TLit "  - ", TVar "."]
  _ ->
    TSeq [TLit "  - ", TVar "."]

-- | Generate inline fields (for list items)
genInlineFields :: ObjectFields -> [TemplateFragment]
genInlineFields fields =
  let prioritized = sortOn (Down . fieldPriority . fst3) fields
      nonMeta = filter (not . isMetaField . fst3) prioritized
      displayed = take 3 nonMeta  -- Show up to 3 key fields
  in case displayed of
    [] -> [TVar "."]
    fs -> interleave (TLit " | ") (map genInlineField fs)
  where
    fst3 (a, _, _) = a

-- | Generate inline field display
genInlineField :: (Text, FieldSchema, Bool) -> TemplateFragment
genInlineField (name, schema, _) = case schema of
  FString -> TVar name
  FInteger -> TVar name
  FNumber -> TVar name
  FBoolean -> TVar name
  FNullable inner -> TSection name [genInlineField (name, inner, False)]
  FObject subFields ->
    -- For nested objects, show first string field
    case [(n, s) | (n, s@FString, _) <- subFields] of
      ((subName, _):_) -> TVar (name <> "." <> subName)
      _ -> TVar name
  _ -> TVar name

-- | Generate completion/done event template
genCompletionTemplate :: Text -> ObjectFields -> TemplateFragment
genCompletionTemplate variant fields =
  TSeq $ [TLit checkmark, TLit " ", TLit (formatVariantName variant)]
    ++ maybeUsage
    ++ [TNewline]
  where
    checkmark = "\x2713"  -- ✓
    maybeUsage = case lookup "usage" [(n, s) | (n, s, _) <- fields] of
      Just (FNullable (FObject usageFields)) ->
        [ TSection "usage"
            [ TLit " ("
            , TVar "total_tokens"
            , TLit " tokens)"
            ]
        ]
      _ -> []

-- | Generate template for a plain object
genObjectTemplate :: ObjectFields -> TemplateFragment
genObjectTemplate fields =
  let prioritized = sortOn (Down . fieldPriority . fst3) fields
      nonMeta = filter (not . isMetaField . fst3) prioritized
  in TSeq $ map genFieldLine nonMeta
  where
    fst3 (a, _, _) = a

-- | Generate a single field line
genFieldLine :: (Text, FieldSchema, Bool) -> TemplateFragment
genFieldLine (name, schema, _required) =
  TSeq [genFieldTemplate name schema, TNewline]

-- | Generate template for a field based on its schema
genFieldTemplate :: Text -> FieldSchema -> TemplateFragment
genFieldTemplate name = \case
  FString ->
    if isContentField name
      then TVar name  -- Content fields: just the value
      else TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

  FInteger ->
    TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

  FNumber ->
    TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

  FBoolean ->
    TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

  FArray itemSchema ->
    TSeq
      [ TLit (formatFieldName name), TLit ":", TNewline
      , TSection name [genListItemTemplate itemSchema, TNewline]
      ]

  FObject subFields ->
    TSeq
      [ TLit (formatFieldName name), TLit ":", TNewline
      , TIndent 2 [genObjectTemplate subFields]
      ]

  FNullable inner ->
    TSection name [genFieldTemplate name inner]

  FConst _ ->
    TSeq []  -- Don't render const discriminators

  FRef _ ->
    TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

  FAny ->
    TSeq [TLit (formatFieldName name), TLit ": ", TVar name]

-- | Generate template for primitive return types
genPrimitiveTemplate :: FieldSchema -> TemplateFragment
genPrimitiveTemplate = \case
  FString -> TVar "."
  FArray inner -> TSection "." [genListItemTemplate inner, TNewline]
  _ -> TVar "."

-- | Generate buffered/combined template for all variants
genBufferedTemplate :: [VariantSchema] -> TemplateFragment
genBufferedTemplate variants =
  TSeq $ map genBufferedVariant variants
  where
    genBufferedVariant v =
      TSection (variantName v) [genVariantTemplate v]

-- ============================================================================
-- Compilation to Mustache
-- ============================================================================

-- | Compile a template fragment to Mustache string
compileFragment :: TemplateFragment -> Text
compileFragment = compileWith 0

-- | Compile with current indentation level
compileWith :: Int -> TemplateFragment -> Text
compileWith indent = \case
  TLit t -> t

  TVar name ->
    "{{{" <> name <> "}}}"

  TEsc name ->
    "{{" <> name <> "}}"

  TSection name body ->
    "{{#" <> name <> "}}" <> compileFragments body <> "{{/" <> name <> "}}"

  TInverted name body ->
    "{{^" <> name <> "}}" <> compileFragments body <> "{{/" <> name <> "}}"

  TSeq frags ->
    T.concat $ map (compileWith indent) frags

  TNewline ->
    "\n" <> T.replicate indent " "

  TIndent n body ->
    compileWith (indent + n) (TSeq body)

-- | Compile multiple fragments
compileFragments :: [TemplateFragment] -> Text
compileFragments = T.concat . map compileFragment

-- ============================================================================
-- Heuristics
-- ============================================================================

-- | Priority for field display (higher = more important)
fieldPriority :: Text -> Int
fieldPriority name
  | isContentField name = 100
  | name == "name" = 90
  | name == "message" = 85
  | name == "id" = 80
  | name == "status" = 70
  | name == "error" = 60
  | "_id" `T.isSuffixOf` name = 50
  | otherwise = 0

-- | Is this a content field that should be displayed prominently?
isContentField :: Text -> Bool
isContentField name = name `elem` ["content", "text", "body", "output", "result"]

-- | Is this a meta field that should be hidden by default?
isMetaField :: Text -> Bool
isMetaField name = name `elem`
  [ "type"           -- Discriminator
  , "created_at"
  , "updated_at"
  , "plexus_hash"
  , "provenance"
  ]

-- | Format field name for display (snake_case -> Title Case)
formatFieldName :: Text -> Text
formatFieldName = T.intercalate " " . map T.toTitle . T.splitOn "_"

-- | Format variant name for display (snake_case -> Title Case)
formatVariantName :: Text -> Text
formatVariantName = T.intercalate " " . map T.toTitle . T.splitOn "_"

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Interleave a separator between elements
interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave sep (x:xs) = x : sep : interleave sep xs
