{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Template-based output renderer for CLI responses
--
-- Provides runtime-configurable output rendering using Mustache templates.
-- Templates can be loaded from:
--   1. Project-local: .substrate/templates/
--   2. User global: ~/.config/symbols/templates/
--   3. Schema-generated defaults (from return type)
--   4. Built-in JSON fallback
module Plexus.Renderer
  ( -- * Types
    RendererConfig(..)
  , OutputFormat(..)
  , RenderError(..)
  , Template
    -- * Configuration
  , defaultConfig
  , defaultSearchPaths
    -- * Template Loading
  , loadTemplate
  , loadTemplateText
  , resolveTemplate
    -- * Rendering
  , render
  , renderValue
  , renderWithDefault
  , renderWithSchema
    -- * Utilities
  , formatJson
  , formatPrettyJson
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.Encode.Pretty as AP
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>), (<.>))
import Text.Mustache (Template, compileMustacheText, renderMustache)

-- ============================================================================
-- Types
-- ============================================================================

-- | Output format selection
data OutputFormat
  = FormatJson           -- ^ Raw JSON (compact)
  | FormatPrettyJson     -- ^ Pretty-printed JSON
  | FormatTemplate       -- ^ Use Mustache templates
  deriving stock (Show, Eq)

-- | Configuration for the renderer
data RendererConfig = RendererConfig
  { searchPaths    :: [FilePath]    -- ^ Template search paths (first match wins)
  , useColor       :: Bool          -- ^ Enable ANSI color codes
  , defaultFormat  :: OutputFormat  -- ^ Fallback when no template found
  }
  deriving stock (Show, Eq)

-- | Rendering errors
data RenderError
  = TemplateNotFound Text
  | TemplateParseError Text
  | FileReadError FilePath Text
  deriving stock (Show, Eq)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Default renderer configuration
defaultConfig :: IO RendererConfig
defaultConfig = do
  paths <- defaultSearchPaths
  pure RendererConfig
    { searchPaths   = paths
    , useColor      = True
    , defaultFormat = FormatPrettyJson
    }

-- | Default template search paths
-- Order: project-local, user-global
defaultSearchPaths :: IO [FilePath]
defaultSearchPaths = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  pure
    [ cwd </> ".substrate" </> "templates"
    , home </> ".config" </> "symbols" </> "templates"
    ]

-- ============================================================================
-- Template Loading
-- ============================================================================

-- | Load a template from a file path
loadTemplate :: FilePath -> IO (Either RenderError Template)
loadTemplate path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ TemplateNotFound (T.pack path)
    else do
      result <- (Right <$> TIO.readFile path) `catch` handleError
      pure $ case result of
        Left err -> Left err
        Right content -> case compileMustacheText "template" content of
          Left err -> Left $ TemplateParseError (T.pack $ show err)
          Right tmpl -> Right tmpl
  where
    handleError :: SomeException -> IO (Either RenderError Text)
    handleError e = pure $ Left $ FileReadError path (T.pack $ show e)

-- | Load a template from raw text
loadTemplateText :: Text -> Template
loadTemplateText content = case compileMustacheText "template" content of
  Left err -> error $ "Template parse error: " <> show err
  Right tmpl -> tmpl

-- | Resolve template path for a method
-- Searches in order: namespace/method.mustache, namespace/method.hbs, default.mustache
resolveTemplate :: RendererConfig
                -> Text              -- ^ Namespace (e.g., "arbor")
                -> Text              -- ^ Method (e.g., "tree_list")
                -> IO (Maybe FilePath)
resolveTemplate config namespace method = do
  let methodName = T.replace "_" "-" method  -- tree_list -> tree-list
      candidates = concat
        [ [ path </> T.unpack namespace </> T.unpack methodName <.> ext
          | path <- searchPaths config
          , ext <- ["mustache", "hbs"]
          ]
        , [ path </> "default" <.> ext
          | path <- searchPaths config
          , ext <- ["mustache", "hbs"]
          ]
        ]
  findFirst candidates

-- | Find the first existing file in a list
findFirst :: [FilePath] -> IO (Maybe FilePath)
findFirst [] = pure Nothing
findFirst (p:ps) = do
  exists <- doesFileExist p
  if exists then pure (Just p) else findFirst ps

-- ============================================================================
-- Rendering
-- ============================================================================

-- | Render a JSON value using the appropriate template
render :: RendererConfig
       -> Text              -- ^ Namespace
       -> Text              -- ^ Method
       -> Value             -- ^ JSON response
       -> IO (Either RenderError Text)
render config namespace method value = do
  mPath <- resolveTemplate config namespace method
  case mPath of
    Nothing -> pure $ Right $ renderWithFormat (defaultFormat config) value
    Just path -> do
      eTemplate <- loadTemplate path
      case eTemplate of
        Left err -> pure $ Left err
        Right template -> pure $ renderValue template value

-- | Render a value with a template
renderValue :: Template -> Value -> Either RenderError Text
renderValue template value =
  Right $ TL.toStrict $ renderMustache template value

-- | Render with default format (no template)
renderWithDefault :: OutputFormat -> Value -> Text
renderWithDefault = renderWithFormat

-- ============================================================================
-- JSON Formatting
-- ============================================================================

-- | Format JSON based on output format
renderWithFormat :: OutputFormat -> Value -> Text
renderWithFormat FormatJson = formatJson
renderWithFormat FormatPrettyJson = formatPrettyJson
renderWithFormat FormatTemplate = formatPrettyJson  -- Fallback

-- | Compact JSON format
formatJson :: Value -> Text
formatJson = TE.decodeUtf8 . LBS.toStrict . encode

-- | Pretty-printed JSON format
formatPrettyJson :: Value -> Text
formatPrettyJson = TE.decodeUtf8 . LBS.toStrict . AP.encodePretty

-- ============================================================================
-- Schema-based Rendering
-- ============================================================================

-- | Render a value with schema-generated templates as fallback
--
-- Priority:
--   1. User-defined template file
--   2. Schema-generated template (from return type)
--   3. Pretty JSON fallback
--
-- Takes the return schema JSON value from plexus_full_schema.
renderWithSchema :: RendererConfig
                 -> Text              -- ^ Namespace
                 -> Text              -- ^ Method
                 -> Maybe Value       -- ^ Return schema from plexus_full_schema
                 -> Value             -- ^ Data to render
                 -> IO (Either RenderError Text)
renderWithSchema config namespace method mReturnSchema value = do
  -- First, try user-defined template
  mPath <- resolveTemplate config namespace method
  case mPath of
    Just path -> loadTemplate path >>= \case
      Right template -> pure $ renderValue template value
      Left _ -> trySchemaFallback

    Nothing -> trySchemaFallback
  where
    trySchemaFallback = case mReturnSchema of
      Nothing -> pure $ Right $ renderWithFormat (defaultFormat config) value
      Just returnSchema ->
        case parseAndRender returnSchema value of
          Just rendered -> pure $ Right rendered
          Nothing -> pure $ Right $ renderWithFormat (defaultFormat config) value

    -- Parse schema and render with generated template
    parseAndRender :: Value -> Value -> Maybe Text
    parseAndRender schema val = do
      -- Import inline to avoid circular deps - this could be refactored
      -- For now, we just extract the variant type and use it
      variantType <- getVariantType val
      -- Generate simple inline template based on variant
      Just $ renderVariantSimple variantType val

    -- Get the "type" field from the value to determine variant
    getVariantType :: Value -> Maybe Text
    getVariantType (Object o) = case KM.lookup (K.fromText "type") o of
      Just (String t) -> Just t
      _ -> Nothing
    getVariantType _ = Nothing

    -- Simple variant-based rendering without full schema parsing
    -- This is a lightweight inline version; for full power, use Plexus.Template
    renderVariantSimple :: Text -> Value -> Text
    renderVariantSimple variantType val = case variantType of
      -- Chat streaming: just output content
      "chat_content" -> extractField "content" val
      "content" -> extractField "content" val

      -- Completion events: checkmark with minimal info
      "chat_complete" -> "\x2713 Complete" <> maybeUsage val <> "\n"
      "complete" -> "\x2713 Complete\n"

      -- Error events
      "error" -> "Error: " <> extractField "message" val <> "\n"

      -- List events: try to format as list (check variant name or if value has array)
      _ | "_list" `T.isSuffixOf` variantType -> renderListVariant val
        | hasArrayField val -> renderListVariant val

      -- Default: pretty JSON
      _ -> formatPrettyJson val

    -- Check if value contains an array field (indicates list-like data)
    hasArrayField :: Value -> Bool
    hasArrayField (Object o) = any isArrayValue (KM.elems o)
    hasArrayField _ = False

    isArrayValue :: Value -> Bool
    isArrayValue (Array _) = True
    isArrayValue _ = False

    extractField :: Text -> Value -> Text
    extractField field (Object o) = case KM.lookup (K.fromText field) o of
      Just (String t) -> t
      Just v -> formatPrettyJson v
      Nothing -> ""
    extractField _ _ = ""

    maybeUsage :: Value -> Text
    maybeUsage (Object o) = case KM.lookup (K.fromText "usage") o of
      Just (Object usage) -> case KM.lookup (K.fromText "total_tokens") usage of
        Just (Number n) -> " (" <> T.pack (show (round n :: Int)) <> " tokens)"
        _ -> ""
      _ -> ""
    maybeUsage _ = ""

    renderListVariant :: Value -> Text
    renderListVariant (Object o) =
      -- Find the array field
      case [(k, arr) | (k, Array arr) <- KM.toList o, K.toText k /= "type"] of
        [(_, items)] ->
          T.unlines $ map renderListItem (toList items)
        _ -> formatPrettyJson (Object o)
    renderListVariant v = formatPrettyJson v

    renderListItem :: Value -> Text
    renderListItem (Object o) =
      -- Try to find a name or id field
      let name = case KM.lookup (K.fromText "name") o of
            Just (String n) -> n
            _ -> case KM.lookup (K.fromText "id") o of
              Just (String i) -> i
              _ -> formatPrettyJson (Object o)
      in "  - " <> name
    renderListItem v = "  - " <> formatPrettyJson v

    toList arr = foldr (:) [] arr
