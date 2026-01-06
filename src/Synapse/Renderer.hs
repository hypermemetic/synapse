{-# LANGUAGE TemplateHaskell #-}

-- | Template-based output renderer
--
-- Runtime-configurable output rendering using Mustache templates.
-- Templates map content_type to human-readable output.
--
-- = Template Resolution
--
-- Templates are searched in order:
-- 1. Project-local: @.substrate/templates/{namespace}/{method}.mustache@
-- 2. User global: @~/.config/synapse/templates/{namespace}/{method}.mustache@
-- 3. Built-in defaults
--
-- = Usage
--
-- @
-- cfg <- defaultRendererConfig
-- result <- renderItem cfg item
-- case result of
--   Just text -> TIO.putStrLn text
--   Nothing   -> printJson item  -- fallback
-- @
module Synapse.Renderer
  ( -- * Configuration
    RendererConfig(..)
  , defaultRendererConfig
  , OutputMode(..)

    -- * Rendering
  , renderItem
  , renderValue
  , renderWithTemplate
  , prettyValue

    -- * Template Resolution
  , resolveTemplate
  , templateSearchPaths

    -- * Template Loading
  , loadTemplate
  , TemplateCache
  , newTemplateCache
  , getCachedTemplate
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>), (<.>))
import Text.Mustache (Template, toMustache)
import Text.Mustache.Compile (localAutomaticCompile)
import Text.Mustache.Render (substituteValue)
import qualified Text.Mustache.Types as MT

import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- Types
-- ============================================================================

-- | Output mode for rendering
data OutputMode
  = ModeTemplate      -- ^ Use templates when available
  | ModeJson          -- ^ Raw JSON stream items
  | ModeRaw           -- ^ Just the content value
  deriving stock (Show, Eq)

-- | Renderer configuration
data RendererConfig = RendererConfig
  { rcSearchPaths :: [FilePath]     -- ^ Template search paths
  , rcMode        :: OutputMode     -- ^ Output mode
  , rcCache       :: TemplateCache  -- ^ Template cache
  }

-- | Template cache to avoid re-parsing
newtype TemplateCache = TemplateCache (IORef (Map FilePath Template))

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Create a new template cache
newTemplateCache :: IO TemplateCache
newTemplateCache = TemplateCache <$> newIORef Map.empty

-- | Get default renderer configuration
defaultRendererConfig :: IO RendererConfig
defaultRendererConfig = do
  paths <- templateSearchPaths
  cache <- newTemplateCache
  pure $ RendererConfig
    { rcSearchPaths = paths
    , rcMode = ModeTemplate
    , rcCache = cache
    }

-- | Get template search paths
templateSearchPaths :: IO [FilePath]
templateSearchPaths = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  pure
    [ cwd </> ".substrate" </> "templates"
    , home </> ".config" </> "synapse" </> "templates"
    ]

-- ============================================================================
-- Template Resolution
-- ============================================================================

-- | Resolve template path for a content type
-- Content type format: "namespace.method" (e.g., "echo.once", "arbor.tree_list")
-- Search order:
--   1. {searchPath}/{namespace}/{event}.mustache (exact match)
--   2. {searchPath}/{namespace}/default.mustache (namespace default)
--   3. {searchPath}/default.mustache (global default)
resolveTemplate :: RendererConfig -> Text -> IO (Maybe FilePath)
resolveTemplate cfg contentType = do
  let (namespace, method) = parseContentType contentType
  let candidates =
        -- Exact event match
        [ path </> T.unpack namespace </> T.unpack method <.> "mustache"
        | path <- rcSearchPaths cfg
        ]
        -- Namespace default
        ++ [ path </> T.unpack namespace </> "default" <.> "mustache"
           | path <- rcSearchPaths cfg
           ]
        -- Global default
        ++ [ path </> "default.mustache" | path <- rcSearchPaths cfg ]
  firstExisting candidates

-- | Parse content_type into (namespace, method)
parseContentType :: Text -> (Text, Text)
parseContentType ct = case T.splitOn "." ct of
  [ns, m] -> (ns, m)
  [m]     -> ("default", m)
  parts   -> (T.intercalate "." (init parts), last parts)

-- | Find first existing file from candidates
firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (p:ps) = do
  exists <- doesFileExist p
  if exists then pure (Just p) else firstExisting ps

-- ============================================================================
-- Template Loading
-- ============================================================================

-- | Load a template from disk
loadTemplate :: FilePath -> IO (Either Text Template)
loadTemplate path = do
  result <- localAutomaticCompile path
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right template -> pure $ Right template

-- | Get template from cache, loading if needed
getCachedTemplate :: RendererConfig -> FilePath -> IO (Either Text Template)
getCachedTemplate cfg path = do
  let TemplateCache cacheRef = rcCache cfg
  cache <- readIORef cacheRef
  case Map.lookup path cache of
    Just template -> pure $ Right template
    Nothing -> do
      result <- loadTemplate path
      case result of
        Right template -> do
          modifyIORef' cacheRef (Map.insert path template)
          pure $ Right template
        Left err -> pure $ Left err

-- ============================================================================
-- Rendering
-- ============================================================================

-- | Render a stream item using templates
renderItem :: RendererConfig -> PlexusStreamItem -> IO (Maybe Text)
renderItem cfg item = case rcMode cfg of
  ModeJson -> pure Nothing  -- Caller should use JSON
  ModeRaw  -> pure Nothing  -- Caller should extract content
  ModeTemplate -> case item of
    StreamData _ _ contentType content ->
      renderValue cfg contentType content
    StreamProgress _ _ msg _ ->
      pure $ Just msg
    StreamError _ _ err _ ->
      pure $ Just $ "Error: " <> err
    StreamDone _ _ ->
      pure Nothing

-- | Render a value using template for content type
renderValue :: RendererConfig -> Text -> Value -> IO (Maybe Text)
renderValue cfg contentType value = do
  mPath <- resolveTemplate cfg contentType
  case mPath of
    Nothing -> pure Nothing
    Just path -> do
      result <- getCachedTemplate cfg path
      case result of
        Left _err -> pure Nothing
        Right template -> pure $ Just $ renderWithTemplate template value

-- | Render a value with a specific template
renderWithTemplate :: Template -> Value -> Text
renderWithTemplate template value =
  let wrapped = wrapDiscriminatedUnion value
  in substituteValue template (toMustache wrapped)

-- | Wrap discriminated union data for mustache template compatibility
--
-- Templates expect variant data wrapped like: {"echo": {"count": 1, ...}}
-- But actual responses are flat: {"type": "echo", "count": 1, ...}
--
-- This transforms flat discriminated unions into wrapped form so
-- mustache sections like {{#echo}}...{{/echo}} will match.
wrapDiscriminatedUnion :: Value -> Value
wrapDiscriminatedUnion (Object obj) =
  case KM.lookup "type" obj of
    Just (String variant) ->
      -- Wrap: {"variant": original_object}
      Object $ KM.singleton (K.fromText variant) (Object obj)
    _ -> Object obj
wrapDiscriminatedUnion other = other

-- ============================================================================
-- Pretty Printing
-- ============================================================================

-- | Pretty-print a JSON value in human-readable format (no JSON syntax)
prettyValue :: Value -> Text
prettyValue = prettyIndent 0

prettyIndent :: Int -> Value -> Text
prettyIndent indent val = case val of
  Null -> "null"
  Bool True -> "true"
  Bool False -> "false"
  Number n -> case floatingOrInteger n of
    Left d -> T.pack $ show (d :: Double)
    Right i -> T.pack $ show (i :: Integer)
  String s -> s
  Array arr
    | V.null arr -> "[]"
    | otherwise -> T.intercalate "\n" $
        map (\v -> spaces <> "- " <> prettyInline v) (V.toList arr)
  Object obj
    | KM.null obj -> "{}"
    | otherwise -> T.intercalate "\n" $
        map (\(k, v) -> spaces <> K.toText k <> ": " <> prettyChild v) (KM.toList obj)
  where
    spaces = T.replicate indent "  "

    -- For array items and object values, decide inline vs block
    prettyChild v = case v of
      Object o | not (KM.null o) -> "\n" <> prettyIndent (indent + 1) v
      Array a | not (V.null a) -> "\n" <> prettyIndent (indent + 1) v
      _ -> prettyInline v

    -- Inline rendering for simple values
    prettyInline v = case v of
      Null -> "null"
      Bool True -> "true"
      Bool False -> "false"
      Number n -> case floatingOrInteger n of
        Left d -> T.pack $ show (d :: Double)
        Right i -> T.pack $ show (i :: Integer)
      String s -> s
      Array arr -> T.intercalate ", " $ map prettyInline (V.toList arr)
      Object obj -> T.intercalate ", " $
        map (\(k, v') -> K.toText k <> ": " <> prettyInline v') (KM.toList obj)
