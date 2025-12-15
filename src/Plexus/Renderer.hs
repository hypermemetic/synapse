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
  , Template(..)
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
import Control.Monad (guard)
import Data.Aeson (Value(..), encode)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Vector as V
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>), (<.>))

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

-- | A loaded template
data Template = Template
  { templatePath    :: Maybe FilePath  -- ^ Source path (Nothing for built-in)
  , templateContent :: Text            -- ^ Raw template content
  }
  deriving stock (Show, Eq)

-- | Rendering errors
data RenderError
  = TemplateNotFound Text
  | TemplateParseError FilePath Text
  | TemplateRenderError Text
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
      result <- (Right <$> T.readFile path) `catch` handleError
      pure $ case result of
        Left err -> Left err
        Right content -> Right Template
          { templatePath = Just path
          , templateContent = content
          }
  where
    handleError :: SomeException -> IO (Either RenderError Text)
    handleError e = pure $ Left $ FileReadError path (T.pack $ show e)

-- | Load a template from raw text
loadTemplateText :: Text -> Template
loadTemplateText content = Template
  { templatePath = Nothing
  , templateContent = content
  }

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
  Right $ applyTemplate (templateContent template) value

-- | Render with default format (no template)
renderWithDefault :: OutputFormat -> Value -> Text
renderWithDefault = renderWithFormat

-- ============================================================================
-- Template Application (Simple Mustache-style)
-- ============================================================================

-- | Apply a Mustache-style template to a JSON value
-- Supports:
--   {{variable}}              - Simple substitution
--   {{nested.path}}           - Nested path lookup
--   {{#array}}...{{/array}}   - Iterate over arrays
--   {{.}}                     - Current item in iteration
applyTemplate :: Text -> Value -> Text
applyTemplate template value =
  -- First expand sections, then substitute variables
  let expanded = expandSections template value
  in substituteVars expanded value

-- | Expand all {{#section}}...{{/section}} blocks
expandSections :: Text -> Value -> Text
expandSections template value = go template
  where
    go t = case findSection t of
      Nothing -> t
      Just (before, sectionName, body, after) ->
        let sectionValue = lookupValue (T.splitOn "." sectionName) value
            expanded = expandSection sectionValue body
        in go (before <> expanded <> after)

-- | Find the next section in template
-- Returns (before, sectionName, body, after)
findSection :: Text -> Maybe (Text, Text, Text, Text)
findSection t = do
  let (before, rest1) = T.breakOn "{{#" t
  guard (not $ T.null rest1)
  let afterOpen = T.drop 3 rest1
  let (sectionName, rest2) = T.breakOn "}}" afterOpen
  guard (not $ T.null rest2)
  let afterTag = T.drop 2 rest2
  let closeTag = "{{/" <> T.strip sectionName <> "}}"
  let (body, rest3) = T.breakOn closeTag afterTag
  guard (not $ T.null rest3)
  let after = T.drop (T.length closeTag) rest3
  pure (before, T.strip sectionName, body, after)

-- | Expand a section based on its value
expandSection :: Value -> Text -> Text
expandSection (Array arr) body =
  -- Iterate over array elements
  T.concat $ map (expandItem body) (V.toList arr)
expandSection (Bool True) body = body
expandSection (Bool False) _ = ""
expandSection Null _ = ""
expandSection val body =
  -- For objects/other values, just substitute with the value as context
  substituteVars body val

-- | Expand template body for a single array item
expandItem :: Text -> Value -> Text
expandItem body item =
  -- Replace {{.}} with the item, then substitute other vars
  let withDot = T.replace "{{.}}" (valueToText item) body
  in substituteVars withDot item

-- | Substitute all {{var}} patterns in template
substituteVars :: Text -> Value -> Text
substituteVars template value = go template
  where
    go t = case T.breakOn "{{" t of
      (before, "") -> before
      (before, rest) ->
        let afterOpen = T.drop 2 rest
        in case T.breakOn "}}" afterOpen of
          (var, more) ->
            let trimmed = T.strip var
                afterClose = T.drop 2 more
            in if T.isPrefixOf "#" trimmed || T.isPrefixOf "/" trimmed
               then before <> "{{" <> var <> "}}" <> go afterClose
               else before <> lookupPath (T.splitOn "." trimmed) value <> go afterClose

-- | Look up a value by path (returns Value, not Text)
lookupValue :: [Text] -> Value -> Value
lookupValue [] v = v
lookupValue (k:ks) (Object obj) =
  case KM.lookup (fromText k) obj of
    Just v -> lookupValue ks v
    Nothing -> Null
lookupValue (k:ks) (Array arr) =
  case reads (T.unpack k) of
    [(idx, "")] | idx >= 0 && idx < V.length arr ->
      lookupValue ks (arr V.! idx)
    _ -> Null
lookupValue _ _ = Null

-- | Look up a nested path in a JSON value (returns Text)
lookupPath :: [Text] -> Value -> Text
lookupPath path value = valueToText (lookupValue path value)

-- | Convert a JSON value to display text
valueToText :: Value -> Text
valueToText (String s) = s
valueToText (Number n) = case floatingOrInteger n of
  Left (d :: Double) -> T.pack $ show d
  Right (i :: Integer) -> T.pack $ show i
valueToText (Bool True) = "true"
valueToText (Bool False) = "false"
valueToText Null = ""
valueToText v@(Array _) = formatJson v
valueToText v@(Object _) = formatJson v

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
