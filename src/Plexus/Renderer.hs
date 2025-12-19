{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Template-based output renderer for CLI responses
--
-- Provides runtime-configurable output rendering using Mustache templates.
-- Templates can be loaded from:
--   1. Project-local: .substrate/templates/
--   2. User global: ~/.config/symbols/templates/
--   3. Built-in defaults (JSON)
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
    -- * Utilities
  , formatJson
  , formatPrettyJson
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
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
