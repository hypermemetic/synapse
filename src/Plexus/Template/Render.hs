{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Apply generated templates to stream data
--
-- This module handles the runtime application of templates to actual data,
-- supporting both streaming (per-event) and buffered (collected) modes.
--
-- For streaming mode:
--   Each event is rendered immediately as it arrives using the variant-specific
--   template based on the discriminator field (usually "type").
--
-- For buffered mode:
--   Events are accumulated in a buffer, then rendered as a unified view when
--   the stream completes. This is useful for table displays, summaries, etc.
module Plexus.Template.Render
  ( -- * Types
    StreamRenderer(..)
  , BufferState(..)
  , RenderResult(..)
    -- * Rendering
  , renderEvent
  , renderBuffered
  , newBufferState
  , bufferEvent
  , flushBuffer
    -- * Template Application
  , applyTemplate
  , getVariantType
    -- * Integration
  , renderStreamItem
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Text.Mustache (Template, compileMustacheText, renderMustache)

import Plexus.Template.Gen (TemplateSet(..), RenderMode(..))

-- ============================================================================
-- Types
-- ============================================================================

-- | Result of rendering an event
data RenderResult
  = Rendered Text              -- ^ Successfully rendered output
  | WasBuffered                -- ^ Event buffered, no output yet
  | NoTemplate                 -- ^ No template found for this variant
  | RenderError Text           -- ^ Rendering failed
  deriving stock (Show, Eq)

-- | Streaming renderer that applies templates to events
data StreamRenderer = StreamRenderer
  { srTemplates     :: TemplateSet
  , srCompiled      :: Map Text Template    -- ^ Compiled mustache templates
  , srDiscriminator :: Text                 -- ^ Field to discriminate variants
  }

-- | Buffer state for buffered rendering
data BufferState = BufferState
  { bsEvents   :: [Value]       -- ^ Collected events (in order)
  , bsRenderer :: StreamRenderer
  }

-- ============================================================================
-- Construction
-- ============================================================================

-- | Create a new stream renderer from a template set
mkStreamRenderer :: Text -> TemplateSet -> StreamRenderer
mkStreamRenderer discriminator ts = StreamRenderer
  { srTemplates = ts
  , srCompiled = Map.mapMaybe compileTemplate (tsVariants ts)
  , srDiscriminator = discriminator
  }
  where
    compileTemplate :: Text -> Maybe Template
    compileTemplate tmplText =
      case compileMustacheText "template" tmplText of
        Left _ -> Nothing
        Right t -> Just t

-- | Create a new buffer state for buffered rendering
newBufferState :: Text -> TemplateSet -> BufferState
newBufferState discriminator ts = BufferState
  { bsEvents = []
  , bsRenderer = mkStreamRenderer discriminator ts
  }

-- ============================================================================
-- Streaming Rendering
-- ============================================================================

-- | Render a single event using streaming mode
renderEvent :: StreamRenderer -> Value -> RenderResult
renderEvent sr val = case getVariantType (srDiscriminator sr) val of
  Nothing -> NoTemplate
  Just variantType ->
    case Map.lookup variantType (srCompiled sr) of
      Nothing -> NoTemplate
      Just tmpl -> Rendered $ applyTemplate tmpl val

-- | Get the variant type from an event value
getVariantType :: Text -> Value -> Maybe Text
getVariantType field val = case val of
  Object o -> case KM.lookup (K.fromText field) o of
    Just (String t) -> Just t
    _ -> Nothing
  _ -> Nothing

-- | Apply a compiled template to a value
applyTemplate :: Template -> Value -> Text
applyTemplate tmpl val = TL.toStrict $ renderMustache tmpl val

-- ============================================================================
-- Buffered Rendering
-- ============================================================================

-- | Buffer an event for later rendering
bufferEvent :: BufferState -> Value -> BufferState
bufferEvent bs val = bs { bsEvents = bsEvents bs ++ [val] }

-- | Flush the buffer and render all collected events
flushBuffer :: BufferState -> RenderResult
flushBuffer bs = case tsBuffered (srTemplates (bsRenderer bs)) of
  Nothing ->
    -- No buffered template, render each event individually
    let rendered = map (renderEvent (bsRenderer bs)) (bsEvents bs)
        texts = [t | Rendered t <- rendered]
    in Rendered $ T.concat texts

  Just bufferedTmpl ->
    -- Use the combined template
    case compileMustacheText "buffered" bufferedTmpl of
      Left err -> RenderError $ T.pack $ show err
      Right tmpl ->
        -- Create combined context with all events grouped by type
        let grouped = groupByVariant (srDiscriminator (bsRenderer bs)) (bsEvents bs)
        in Rendered $ applyTemplate tmpl (Object $ KM.fromList grouped)

-- | Render buffered events into final output
renderBuffered :: BufferState -> Text
renderBuffered bs = case flushBuffer bs of
  Rendered t -> t
  RenderError e -> "Render error: " <> e
  _ -> ""

-- | Group events by their variant type
groupByVariant :: Text -> [Value] -> [(K.Key, Value)]
groupByVariant field events =
  [ (K.fromText variantType, Array $ toArray evts)
  | (variantType, evts) <- Map.toList grouped
  ]
  where
    grouped = foldr addEvent Map.empty events
    addEvent evt m = case getVariantType field evt of
      Nothing -> m
      Just vt -> Map.insertWith (++) vt [evt] m
    toArray = foldr (\x acc -> x `V.cons` acc) V.empty

-- ============================================================================
-- Integration with Plexus Stream
-- ============================================================================

-- | Render a PlexusStreamItem's data field
-- This is the main entry point for CLI integration
renderStreamItem :: StreamRenderer -> Value -> Maybe Text
renderStreamItem sr val = case renderEvent sr val of
  Rendered t -> Just t
  _ -> Nothing

-- ============================================================================
-- Convenience Functions
-- ============================================================================

-- | Create a streaming renderer with default "type" discriminator
defaultStreamRenderer :: TemplateSet -> StreamRenderer
defaultStreamRenderer = mkStreamRenderer "type"

-- | Quick render for one-off use
quickRender :: TemplateSet -> Value -> Text
quickRender ts val =
  let sr = defaultStreamRenderer ts
  in case renderEvent sr val of
    Rendered t -> t
    NoTemplate -> renderFallback val
    RenderError e -> "Error: " <> e
    WasBuffered -> ""

-- | Fallback JSON rendering
renderFallback :: Value -> Text
renderFallback = T.pack . show  -- Could use pretty-printing
