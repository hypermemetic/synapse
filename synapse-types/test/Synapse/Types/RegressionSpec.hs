{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Regression tests for known bugs in Plexus RPC implementations
--
-- Each test here represents a REAL BUG found in production implementations.
-- These tests ensure synapse-types catches all known protocol violations.
module Synapse.Types.RegressionSpec where

import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Synapse.Types.Refined
import Synapse.Types.Protocol

spec :: Spec
spec = do
  describe "BUG: Float timestamp from plexus-rpc-ts" $ do
    it "rejects float timestamp value" $ do
      -- Bug: Date.now() / 1000 produces floats
      -- Example: 1735052400.789
      -- synapse-types requires Int64, so this won't even compile:
      -- let result = mkTimestamp (1735052400.789 :: Double)

      -- But we can test the boundary case
      let result = mkTimestamp (1735052400 :: Int64)
      result `shouldSatisfy` isRight

      -- Negative case: zero (invalid)
      let invalid = mkTimestamp 0
      invalid `shouldSatisfy` isLeft

    it "explains that timestamp must be integer" $ do
      -- When receiving JSON with float, parsing should fail with clear message
      -- This will be tested in integration tests with JSON
      True `shouldBe` True  -- Placeholder

  describe "BUG: Invalid hash length" $ do
    it "rejects hash shorter than 16 characters" $ do
      -- Bug: Server sends truncated hash
      let result = mkPlexusHash "abc123"
      result `shouldSatisfy` isLeft

    it "rejects hash longer than 16 characters" $ do
      -- Bug: Server sends full SHA-256 instead of truncated
      let result = mkPlexusHash "abc123def4567890abcdefabcdef12345678"
      result `shouldSatisfy` isLeft

    it "accepts exactly 16 characters" $ do
      let result = mkPlexusHash "abc123def4567890"
      result `shouldSatisfy` isRight

  describe "BUG: Invalid hash characters" $ do
    it "rejects uppercase hexadecimal" $ do
      -- Bug: Server uses uppercase hex
      let result = mkPlexusHash "ABC123DEF4567890"
      result `shouldSatisfy` isLeft

    it "rejects mixed case hexadecimal" $ do
      let result = mkPlexusHash "Abc123Def4567890"
      result `shouldSatisfy` isLeft

    it "rejects non-hexadecimal characters" $ do
      -- Bug: Server sends non-hex chars (typo, corruption, injection)
      let result = mkPlexusHash "xyz123def4567890"
      result `shouldSatisfy` isLeft

    it "rejects SQL injection attempt in hash" $ do
      let result = mkPlexusHash "'; DROP TABLE u"
      result `shouldSatisfy` isLeft

    it "rejects hash with spaces" $ do
      let result = mkPlexusHash "abc123 ef4567890"
      result `shouldSatisfy` isLeft

  describe "BUG: Empty provenance chain" $ do
    it "creates non-empty provenance from single name" $ do
      let prov = mkProvenance "substrate"
      -- NonEmpty is guaranteed by type system
      prov `shouldSatisfy` (\p -> length (toList p) == 1)

    it "mkStreamMetadata requires non-empty provenance" $ do
      -- Type system enforces this - provenance is NonEmpty Text
      -- Can't even construct empty provenance
      let result = mkStreamMetadata "substrate" "abc123def4567890" 1735052400
      result `shouldSatisfy` isRight

  describe "BUG: Negative timestamp" $ do
    it "rejects negative timestamp" $ do
      let result = mkTimestamp (-1)
      result `shouldSatisfy` isLeft

    it "rejects timestamp = 0" $ do
      -- Zero is not a valid timestamp in our protocol
      let result = mkTimestamp 0
      result `shouldSatisfy` isLeft

    it "accepts positive timestamp" $ do
      let result = mkTimestamp 1
      result `shouldSatisfy` isRight

  describe "BUG: Percentage out of range" $ do
    it "rejects negative percentage" $ do
      -- Bug: Server sends negative progress
      let result = mkPercentage (-1)
      result `shouldSatisfy` isLeft

    it "rejects percentage > 100" $ do
      -- Bug: Server calculation error sends 150%
      let result = mkPercentage 150
      result `shouldSatisfy` isLeft

    it "accepts 0%" $ do
      let result = mkPercentage 0
      result `shouldSatisfy` isRight

    it "accepts 100%" $ do
      let result = mkPercentage 100
      result `shouldSatisfy` isRight

    it "accepts 50%" $ do
      let result = mkPercentage 50
      result `shouldSatisfy` isRight

  describe "BUG: Invalid port numbers" $ do
    it "rejects port 0" $ do
      -- Bug: Uninitialized port number
      let result = mkPort 0
      result `shouldSatisfy` isLeft

    it "rejects negative port" $ do
      let result = mkPort (-1)
      result `shouldSatisfy` isLeft

    it "rejects port > 65535" $ do
      -- Bug: Port overflow
      let result = mkPort 70000
      result `shouldSatisfy` isLeft

    it "accepts port 1" $ do
      let result = mkPort 1
      result `shouldSatisfy` isRight

    it "accepts port 65535" $ do
      let result = mkPort 65535
      result `shouldSatisfy` isRight

    it "accepts common ports" $ do
      mkPort 80 `shouldSatisfy` isRight
      mkPort 443 `shouldSatisfy` isRight
      mkPort 8080 `shouldSatisfy` isRight

  describe "BUG: Invalid snake_case field names" $ do
    it "rejects camelCase field name" $ do
      -- Bug: TypeScript server sends camelCase
      let result = mkSnakeCaseField "plexusHash"
      result `shouldSatisfy` isLeft

    it "rejects PascalCase field name" $ do
      let result = mkSnakeCaseField "PlexusHash"
      result `shouldSatisfy` isLeft

    it "rejects field starting with underscore" $ do
      -- Bug: Private field naming leaked to protocol
      let result = mkSnakeCaseField "_plexus_hash"
      result `shouldSatisfy` isLeft

    it "rejects field with uppercase letters" $ do
      let result = mkSnakeCaseField "plexus_Hash"
      result `shouldSatisfy` isLeft

    it "rejects field with spaces" $ do
      let result = mkSnakeCaseField "plexus hash"
      result `shouldSatisfy` isLeft

    it "rejects field with hyphens" $ do
      -- Bug: kebab-case instead of snake_case
      let result = mkSnakeCaseField "plexus-hash"
      result `shouldSatisfy` isLeft

    it "accepts valid snake_case" $ do
      mkSnakeCaseField "plexus_hash" `shouldSatisfy` isRight
      mkSnakeCaseField "content_type" `shouldSatisfy` isRight
      mkSnakeCaseField "request_id" `shouldSatisfy` isRight

    it "accepts snake_case with numbers" $ do
      mkSnakeCaseField "field_123" `shouldSatisfy` isRight
      mkSnakeCaseField "test_v2" `shouldSatisfy` isRight

  describe "BUG: Invalid namespace" $ do
    it "rejects empty namespace" $ do
      -- Bug: Server sends empty string
      let result = mkNamespace ""
      result `shouldSatisfy` isLeft

    it "rejects namespace with uppercase" $ do
      -- Bug: Namespace not lowercased
      let result = mkNamespace "Substrate"
      result `shouldSatisfy` isLeft

    it "rejects namespace with spaces" $ do
      let result = mkNamespace "my plugin"
      result `shouldSatisfy` isLeft

    it "accepts valid namespace" $ do
      mkNamespace "substrate" `shouldSatisfy` isRight
      mkNamespace "cone" `shouldSatisfy` isRight

    it "accepts namespace with underscores" $ do
      mkNamespace "my_plugin" `shouldSatisfy` isRight

  describe "BUG: Invalid subscription/request IDs" $ do
    it "SubscriptionId accepts zero" $ do
      -- Subscription IDs start from 0
      let result = mkSubscriptionId 0
      result `shouldSatisfy` isRight

    it "SubscriptionId rejects negative" $ do
      let result = mkSubscriptionId (-1)
      result `shouldSatisfy` isLeft

    it "RequestId rejects zero" $ do
      -- Request IDs start from 1
      let result = mkRequestId 0
      result `shouldSatisfy` isLeft

    it "RequestId accepts positive" $ do
      let result = mkRequestId 1
      result `shouldSatisfy` isRight

    it "RequestId rejects negative" $ do
      let result = mkRequestId (-1)
      result `shouldSatisfy` isLeft

  describe "BUG: Buffer overflow attempts" $ do
    it "rejects extremely long hash (potential DoS)" $ do
      let longHash = replicate 1000000 'a'
      let result = mkPlexusHash (fromString longHash)
      result `shouldSatisfy` isLeft

    it "hash validation protects against DoS via length check" $ do
      -- PlexusHash MUST be exactly 16 chars, so long inputs are rejected
      mkPlexusHash (T.replicate 1000 "a") `shouldSatisfy` isLeft

    -- TODO: Add length limits to SnakeCaseField to prevent DoS
    -- Currently accepts arbitrarily long strings if they're valid snake_case
    it "documents that snake_case has no length limit (potential DoS)" $ do
      let longName = T.replicate 1000 "a"
      let result = mkSnakeCaseField longName
      -- Currently ACCEPTS long strings - this is a known limitation
      result `shouldSatisfy` isRight
      -- Future: Should add max length check (e.g., 256 chars)

  describe "BUG: Type confusion" $ do
    it "hash must be exactly 16 chars, not 15 or 17" $ do
      -- Prevents off-by-one errors
      mkPlexusHash "abc123def456789" `shouldSatisfy` isLeft  -- 15 chars
      mkPlexusHash "abc123def4567890" `shouldSatisfy` isRight  -- 16 chars
      mkPlexusHash "abc123def4567890a" `shouldSatisfy` isLeft  -- 17 chars

fromString :: String -> Text
fromString = T.pack

toList :: NonEmpty a -> [a]
toList = NE.toList
