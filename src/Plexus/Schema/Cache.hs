-- | Schema caching for the dynamic CLI
--
-- Caches the PlexusSchema to disk to avoid fetching on every invocation.
module Plexus.Schema.Cache
  ( -- * Types
    CachedSchema(..)
  , CacheConfig(..)
    -- * Cache Operations
  , defaultCacheConfig
  , defaultCachePath
  , loadCache
  , saveCache
  , isFresh
  , loadSchemaWithCache
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>), takeDirectory)

import Plexus.Schema (PlexusSchema)

-- ============================================================================
-- Types
-- ============================================================================

-- | Cached schema with metadata
data CachedSchema = CachedSchema
  { cachedAt     :: UTCTime       -- ^ When the schema was cached
  , cachedTTL    :: Int           -- ^ TTL in seconds
  , cachedSchema :: PlexusSchema  -- ^ The cached schema
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CachedSchema where
  parseJSON = withObject "CachedSchema" $ \o ->
    CachedSchema
      <$> o .: "cached_at"
      <*> o .: "ttl"
      <*> o .: "schema"

instance ToJSON CachedSchema where
  toJSON CachedSchema{..} = object
    [ "cached_at" .= cachedAt
    , "ttl"       .= cachedTTL
    , "schema"    .= cachedSchema
    ]

-- | Cache configuration
data CacheConfig = CacheConfig
  { cachePath    :: FilePath  -- ^ Path to cache file
  , cacheTTLSecs :: Int       -- ^ TTL in seconds (default 3600 = 1 hour)
  }
  deriving stock (Show, Eq)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Default cache configuration
-- Uses ~/.cache/symbols/schema.json on Linux/macOS
defaultCacheConfig :: IO CacheConfig
defaultCacheConfig = do
  path <- defaultCachePath
  pure CacheConfig
    { cachePath    = path
    , cacheTTLSecs = 3600  -- 1 hour
    }

-- | Get the default cache file path
-- Respects XDG_CACHE_HOME on Linux/macOS
defaultCachePath :: IO FilePath
defaultCachePath = do
  cacheDir <- getXdgDirectory XdgCache "symbols"
  pure $ cacheDir </> "schema.json"

-- ============================================================================
-- Cache Operations
-- ============================================================================

-- | Load cached schema from disk
loadCache :: FilePath -> IO (Maybe CachedSchema)
loadCache path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- (Just <$> LBS.readFile path) `catch` \(_ :: SomeException) -> pure Nothing
      case result of
        Nothing -> pure Nothing
        Just bs -> case eitherDecode bs of
          Left _err -> pure Nothing
          Right cached -> pure (Just cached)

-- | Save schema to cache
saveCache :: FilePath -> CachedSchema -> IO ()
saveCache path cached = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encode cached)
    `catch` \(_ :: SomeException) -> pure ()  -- Silently fail on write errors

-- | Check if a cached schema is still fresh
isFresh :: CachedSchema -> IO Bool
isFresh cached = do
  now <- getCurrentTime
  let age = diffUTCTime now (cachedAt cached)
  pure $ age < fromIntegral (cachedTTL cached)

-- | Load schema with caching
--
-- 1. If forceRefresh is True, skip cache and fetch fresh
-- 2. Otherwise, try to load from cache
-- 3. If cache is fresh, return cached schema
-- 4. Otherwise, call the fetch function to get fresh schema and cache it
loadSchemaWithCache
  :: Bool                        -- ^ Force refresh (--refresh flag)
  -> CacheConfig                 -- ^ Cache configuration
  -> IO (Either Text PlexusSchema)  -- ^ Fetch function
  -> IO (Either Text PlexusSchema)
loadSchemaWithCache forceRefresh config fetchSchema = do
  if forceRefresh
    then fetchAndCache
    else do
      mCached <- loadCache (cachePath config)
      case mCached of
        Nothing -> fetchAndCache
        Just cached -> do
          fresh <- isFresh cached
          if fresh
            then pure $ Right (cachedSchema cached)
            else fetchAndCache
  where
    fetchAndCache = do
      result <- fetchSchema
      case result of
        Left err -> do
          -- On fetch failure, try stale cache as fallback
          mCached <- loadCache (cachePath config)
          case mCached of
            Just cached -> pure $ Right (cachedSchema cached)  -- Use stale cache
            Nothing -> pure $ Left err  -- No cache available
        Right schema -> do
          now <- getCurrentTime
          let cached = CachedSchema
                { cachedAt     = now
                , cachedTTL    = cacheTTLSecs config
                , cachedSchema = schema
                }
          saveCache (cachePath config) cached
          pure $ Right schema
