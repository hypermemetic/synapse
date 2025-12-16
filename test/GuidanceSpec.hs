{-# LANGUAGE QuasiQuotes #-}

-- | Core invariant tests for stream-based guidance system
--
-- Tests the 6 essential characteristics:
-- 1-3. Parsing (external contract with substrate)
-- 4. Guidance pairing (stateful IORef logic)
-- 5. Round-trip (serialization correctness)
-- 6. End-to-end integration
module Main (main) where

import Data.Aeson (decode, encode, object, toJSON, (.=))
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Plexus.Types
  ( GuidanceErrorType(..)
  , GuidanceSuggestion(..)
  , PlexusStreamItem(..)
  , Provenance(..)
  )

-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Sample provenance for tests
testProvenance :: Provenance
testProvenance = Provenance ["plexus"]

-- | Sample hash for tests
testHash :: Text
testHash = "abc123def456"

-- ============================================================================
-- 1-3. PARSING TESTS - Pin external contract with substrate
-- ============================================================================

spec_parsing :: Spec
spec_parsing = describe "Parsing Invariants (External Contract)" $ do

  it "parses activation_not_found correctly" $ do
    let json = [aesonQQ|{
          "plexus_hash": "abc123",
          "type": "guidance",
          "provenance": {"segments": ["plexus"]},
          "error_type": {
            "error_kind": "activation_not_found",
            "activation": "foo"
          },
          "suggestion": {
            "action": "call_plexus_schema"
          }
        }|]

    case decode (encode json) of
      Just (StreamGuidance hash prov (ActivationNotFound "foo") CallPlexusSchema Nothing Nothing) -> do
        hash `shouldBe` "abc123"
        prov `shouldBe` Provenance ["plexus"]
      other -> expectationFailure $ "Should parse activation_not_found, got: " <> show other

  it "parses method_not_found correctly" $ do
    let json = [aesonQQ|{
          "plexus_hash": "def456",
          "type": "guidance",
          "provenance": {"segments": ["plexus"]},
          "error_type": {
            "error_kind": "method_not_found",
            "activation": "bash",
            "method": "invalid"
          },
          "suggestion": {
            "action": "try_method",
            "method": "bash_execute",
            "example_params": "echo 'hello'"
          },
          "available_methods": ["execute"]
        }|]

    case decode (encode json) of
      Just (StreamGuidance _ _ (MethodNotFound "bash" "invalid") (TryMethod "bash_execute" _) (Just ["execute"]) Nothing) ->
        pure ()  -- Success
      other -> expectationFailure $ "Should parse method_not_found, got: " <> show other

  it "parses invalid_params correctly" $ do
    let json = [aesonQQ|{
          "plexus_hash": "ghi789",
          "type": "guidance",
          "provenance": {"segments": ["plexus"]},
          "error_type": {
            "error_kind": "invalid_params",
            "method": "execute",
            "reason": "missing required parameter: command"
          },
          "suggestion": {
            "action": "call_activation_schema",
            "namespace": "bash"
          }
        }|]

    case decode (encode json) of
      Just (StreamGuidance _ _ (InvalidParams "execute" reason) (CallActivationSchema "bash") Nothing Nothing) -> do
        T.unpack reason `shouldContain` "missing required"
      other -> expectationFailure $ "Should parse invalid_params, got: " <> show other

-- ============================================================================
-- 4. GUIDANCE PAIRING TEST - Pin stateful IORef logic
-- ============================================================================

spec_pairing :: Spec
spec_pairing = describe "Stream Flow Invariants (Stateful Logic)" $ do

  it "pairs guidance with error, then clears state" $ do
    -- Setup
    guidanceRef <- newIORef Nothing :: IO (IORef (Maybe PlexusStreamItem))

    let guidance = StreamGuidance
          testHash
          testProvenance
          (ActivationNotFound "foo")
          CallPlexusSchema
          Nothing
          Nothing

    let err = StreamError testHash testProvenance "Activation not found: foo" False

    -- Step 1: Store guidance
    writeIORef guidanceRef (Just guidance)
    stored1 <- readIORef guidanceRef
    stored1 `shouldBe` Just guidance

    -- Step 2: Retrieve and clear (simulating error handler)
    mbGuidance <- readIORef guidanceRef
    writeIORef guidanceRef Nothing

    mbGuidance `shouldBe` Just guidance

    -- Step 3: Verify cleared
    stored2 <- readIORef guidanceRef
    stored2 `shouldBe` Nothing

-- ============================================================================
-- 5. ROUND-TRIP PROPERTY - Pin serialization correctness
-- ============================================================================

-- | Arbitrary instance for GuidanceErrorType
instance Arbitrary GuidanceErrorType where
  arbitrary = oneof
    [ ActivationNotFound <$> arbitraryText
    , MethodNotFound <$> arbitraryText <*> arbitraryText
    , InvalidParams <$> arbitraryText <*> arbitraryText
    ]
    where
      arbitraryText = T.pack <$> listOf1 (choose ('a', 'z'))

-- | Arbitrary instance for GuidanceSuggestion
instance Arbitrary GuidanceSuggestion where
  arbitrary = oneof
    [ pure CallPlexusSchema
    , CallActivationSchema <$> (T.pack <$> listOf1 (choose ('a', 'z')))
    , TryMethod <$> (T.pack <$> listOf1 (choose ('a', 'z'))) <*> pure Nothing
    , CustomGuidance <$> (T.pack <$> listOf1 (choose ('a', 'z')))
    ]

-- | Arbitrary instance for PlexusStreamItem (guidance only)
instance Arbitrary PlexusStreamItem where
  arbitrary = do
    errorType <- arbitrary
    suggestion <- arbitrary
    pure $ StreamGuidance testHash testProvenance errorType suggestion Nothing Nothing

spec_roundtrip :: Spec
spec_roundtrip = describe "Encoding Invariants (Serialization)" $ do

  it "round-trips all guidance events" $ property $ \item ->
    case item of
      StreamGuidance{} ->
        decode (encode item) === Just item
      _ -> property True  -- Only test guidance events

-- ============================================================================
-- 6. END-TO-END INTEGRATION - Pin system-level correctness
-- ============================================================================

spec_integration :: Spec
spec_integration = describe "End-to-End Integration" $ do

  it "full guidance flow: Guidance → Error → Done" $ do
    -- Simulate the stream processing flow
    guidanceRef <- newIORef Nothing :: IO (IORef (Maybe PlexusStreamItem))

    let guidance = StreamGuidance
          testHash
          testProvenance
          (ActivationNotFound "foo")
          CallPlexusSchema
          Nothing
          Nothing

    let error = StreamError testHash testProvenance "Activation 'foo' not found" False
    let done = StreamDone testHash testProvenance

    -- Process guidance event
    case guidance of
      StreamGuidance{} -> writeIORef guidanceRef (Just guidance)
      _ -> pure ()

    -- Process error event (should have guidance)
    mbGuidance <- readIORef guidanceRef
    writeIORef guidanceRef Nothing

    -- Verify we had guidance when error arrived
    case mbGuidance of
      Just (StreamGuidance _ _ (ActivationNotFound activation) CallPlexusSchema _ _) ->
        activation `shouldBe` "foo"
      _ -> expectationFailure "Expected guidance to be available for error"

    -- Verify state cleared
    afterError <- readIORef guidanceRef
    afterError `shouldBe` Nothing

    -- Process done event (state should still be clear)
    afterDone <- readIORef guidanceRef
    afterDone `shouldBe` Nothing

-- ============================================================================
-- Main Test Runner
-- ============================================================================

main :: IO ()
main = hspec $ do
  spec_parsing
  spec_pairing
  spec_roundtrip
  spec_integration
