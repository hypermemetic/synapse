-- | Integration test for hash-based cache invalidation
--
-- Verifies that cache automatically refreshes when plexus_hash changes
module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.Directory (removeFile, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Plexus.Schema (PlexusSchema(..), ActivationInfo(..))
import Plexus.Schema.Cache (CachedSchema(..), CacheConfig(..), loadSchemaWithCache, saveCache, loadCache, isFresh)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Test Fixtures
-- ============================================================================

testSchema :: PlexusSchema
testSchema = PlexusSchema
  { schemaActivations = [ActivationInfo "test" "1.0.0" "Test activation" ["method1"]]
  , schemaTotalMethods = 1
  }

-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec
spec = describe "Hash-Based Cache Invalidation" $ do

  it "uses cached schema when hash matches" $ do
    withSystemTempDirectory "cache-test" $ \tmpDir -> do
      let cachePath = tmpDir ++ "/schema.json"
      let config = CacheConfig cachePath
      let oldHash = "hash123"

      -- Save initial cache
      let cached = CachedSchema oldHash testSchema Map.empty
      saveCache cachePath cached

      -- Load with same hash - should use cache, not fetch
      result <- loadSchemaWithCache
        False  -- No force refresh
        config
        (pure $ Right oldHash)  -- Hash matches
        (error "Should not fetch schema!")  -- This should not be called
        (\_ -> error "Should not fetch enriched!")  -- This should not be called

      case result of
        Right loaded -> cachedHash loaded `shouldBe` oldHash
        Left err -> expectationFailure $ "Should use cache, got error: " <> show err

  it "refreshes cache when hash differs" $ do
    withSystemTempDirectory "cache-test" $ \tmpDir -> do
      let cachePath = tmpDir ++ "/schema.json"
      let config = CacheConfig cachePath
      let oldHash = "hash123"
      let newHash = "hash456"

      -- Save initial cache with old hash
      let cached = CachedSchema oldHash testSchema Map.empty
      saveCache cachePath cached

      -- Load with different hash - should refresh
      fetchCalled <- newIORef False
      result <- loadSchemaWithCache
        False
        config
        (pure $ Right newHash)  -- New hash
        (writeIORef fetchCalled True >> pure (Right testSchema))  -- Track fetch
        (\_ -> pure Nothing)

      -- Verify fetch was called
      wasFetched <- readIORef fetchCalled
      wasFetched `shouldBe` True

      -- Verify new hash was saved
      case result of
        Right loaded -> cachedHash loaded `shouldBe` newHash
        Left err -> expectationFailure $ "Should refresh, got error: " <> show err

  it "uses stale cache when hash fetch fails" $ do
    withSystemTempDirectory "cache-test" $ \tmpDir -> do
      let cachePath = tmpDir ++ "/schema.json"
      let config = CacheConfig cachePath
      let oldHash = "hash123"

      -- Save cache
      let cached = CachedSchema oldHash testSchema Map.empty
      saveCache cachePath cached

      -- Try to load when hash fetch fails
      result <- loadSchemaWithCache
        False
        config
        (pure $ Left "Network error")  -- Hash fetch fails
        (error "Should not fetch schema")
        (\_ -> error "Should not fetch enriched")

      -- Should fall back to stale cache
      case result of
        Right loaded -> cachedHash loaded `shouldBe` oldHash
        Left err -> expectationFailure $ "Should use stale cache, got error: " <> show err

  it "isFresh correctly compares hashes" $ do
    let cached = CachedSchema "hash123" testSchema Map.empty
    isFresh "hash123" cached `shouldBe` True
    isFresh "hash456" cached `shouldBe` False

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = hspec spec
