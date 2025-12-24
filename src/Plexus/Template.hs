{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Schema-driven template generation for Plexus responses
--
-- This module provides automatic Mustache template generation based on
-- JSON Schema return types from plexus_full_schema. Templates are generated
-- with sensible defaults:
--
--   - Content/text fields rendered prominently
--   - Arrays rendered as bullet lists
--   - Discriminated unions handled per-variant
--   - Meta fields (timestamps, hashes) hidden by default
--
-- == Rendering Modes
--
-- Two modes are supported:
--
-- [@Streaming@] (default): Each event is rendered immediately as it arrives.
--   Best for chat responses, logs, and real-time output.
--
-- [@Buffered@]: Events are collected and rendered as a unified view when
--   the stream completes. Best for tables, summaries, and reports.
--
-- == Usage
--
-- @
-- -- Parse return schema from plexus_full_schema
-- let Right schema = parseReturnSchema returnsValue
--
-- -- Generate templates
-- let templates = genTemplates "cone_chat" schema
--
-- -- Create renderer
-- let renderer = mkRenderer templates
--
-- -- Render each event
-- forM_ events $ \\event -> do
--   case renderEvent renderer event of
--     Rendered text -> putStr text
--     _ -> pure ()
-- @
--
-- == Customization
--
-- Generated templates can be overridden by placing custom templates in:
--
--   1. @.substrate/templates/{namespace}/{method}.mustache@ (project-local)
--   2. @~/.config/synapse/templates/{namespace}/{method}.mustache@ (user-global)
--
module Plexus.Template
  ( -- * Schema Types
    FieldSchema(..)
  , VariantSchema(..)
  , ReturnSchema(..)
  , ObjectFields

    -- * Schema Parsing
  , parseReturnSchema
  , parseFieldSchema

    -- * Template Generation
  , TemplateSet(..)
  , RenderMode(..)
  , TemplateFragment(..)
  , genTemplates
  , genVariantTemplate

    -- * Rendering
  , StreamRenderer(..)
  , BufferState(..)
  , RenderResult(..)
  , mkRenderer
  , renderEvent
  , renderStreamItem

    -- * Buffered Mode
  , newBufferState
  , bufferEvent
  , flushBuffer
  , renderBuffered

    -- * Utilities
  , fieldSchemaType
  , isStreamingVariant
  , getVariantType
  , compileFragment
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Text.Mustache (Template, compileMustacheText)

import Plexus.Template.Schema
import Plexus.Template.Gen
import Plexus.Template.Render

-- ============================================================================
-- Convenience Functions
-- ============================================================================

-- | Create a renderer from a template set with default "type" discriminator
mkRenderer :: TemplateSet -> StreamRenderer
mkRenderer ts = StreamRenderer
  { srTemplates = ts
  , srCompiled = Map.mapMaybe compileTemplate (tsVariants ts)
  , srDiscriminator = "type"
  }
  where
    compileTemplate :: Text -> Maybe Template
    compileTemplate tmplText =
      case compileMustacheText "template" tmplText of
        Left _ -> Nothing
        Right t -> Just t

-- | Create renderer with custom discriminator field
mkRendererWithDiscriminator :: Text -> TemplateSet -> StreamRenderer
mkRendererWithDiscriminator disc ts = StreamRenderer
  { srTemplates = ts
  , srCompiled = Map.mapMaybe compileTemplate (tsVariants ts)
  , srDiscriminator = disc
  }
  where
    compileTemplate :: Text -> Maybe Template
    compileTemplate tmplText =
      case compileMustacheText "template" tmplText of
        Left _ -> Nothing
        Right t -> Just t
