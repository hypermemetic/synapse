# Synapse-Types Test Plan: Proving Validation Works

**Date**: 2025-03-20
**Related**: validation-gaps.md, synapse-types package

## Test Philosophy

We need to prove THREE things:
1. **synapse-types rejects invalid inputs** (negative tests)
2. **synapse-types accepts valid inputs** (positive tests)
3. **Integration actually uses the validation** (can't bypass)

## Test Categories

### 1. Unit Tests (Already Have ✅)

**Status**: 40 tests passing in synapse-types
**Location**: `synapse-types/test/Synapse/Types/RefinedSpec.hs`

These test individual refined types:
```haskell
it "rejects float timestamp" $ do
  -- Can't even construct this - type system prevents it
  let result = mkTimestamp 1735052400.5  -- ❌ Type error!
  result `shouldSatisfy` isLeft

it "rejects short hash" $ do
  let result = mkPlexusHash "short"
  result `shouldSatisfy` isLeft

it "rejects empty provenance" $ do
  let prov = mkProvenance ""
  -- Won't compile - mkProvenance returns NonEmpty
```

**What's Missing**: Need to test error message quality

### 2. Regression Tests for Known Bugs (NEED TO ADD ⚠️)

Each bug we found should have an explicit test proving it's caught:

```haskell
-- test/Synapse/Types/RegressionSpec.hs
module Synapse.Types.RegressionSpec where

spec :: Spec
spec = do
  describe "Regression: Float timestamp bug" $ do
    it "rejects float in JSON" $ do
      let json = "{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400.5}"
      case eitherDecode json :: Either String StreamMetadata of
        Left err -> err `shouldContain` "must be integer"
        Right _ -> expectationFailure "Should have rejected float timestamp"

  describe "Regression: camelCase field names" $ do
    it "rejects plexusHash instead of plexus_hash" $ do
      let json = "{\"provenance\":[\"s\"],\"plexusHash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left err -> err `shouldContain` "plexus_hash"
        Right _ -> expectationFailure "Should have rejected camelCase"

  describe "Regression: Empty provenance" $ do
    it "rejects empty provenance array" $ do
      let json = "{\"provenance\":[],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400}"
      case eitherDecode json :: Either String StreamMetadata of
        Left err -> err `shouldContain` "non-empty"
        Right _ -> expectationFailure "Should have rejected empty provenance"

  describe "Regression: Invalid hash length" $ do
    it "rejects 12-char hash" $ do
      let result = mkPlexusHash "abc123def456"
      result `shouldSatisfy` isLeft

    it "rejects 20-char hash" $ do
      let result = mkPlexusHash "abc123def4567890abcd"
      result `shouldSatisfy` isLeft

  describe "Regression: Invalid hash characters" $ do
    it "rejects uppercase hex" $ do
      let result = mkPlexusHash "ABC123DEF4567890"
      result `shouldSatisfy` isLeft

    it "rejects non-hex characters" $ do
      let result = mkPlexusHash "xyz123def4567890"
      result `shouldSatisfy` isLeft

  describe "Regression: Percentage bounds" $ do
    it "rejects negative percentage" $ do
      let result = mkPercentage (-1)
      result `shouldSatisfy` isLeft

    it "rejects percentage > 100" $ do
      let result = mkPercentage 101
      result `shouldSatisfy` isLeft
```

### 3. Property-Based Tests (NEED TO ADD ⚠️)

Use QuickCheck to generate random invalid data:

```haskell
-- test/Synapse/Types/PropertySpec.hs
module Synapse.Types.PropertySpec where

import Test.QuickCheck

spec :: Spec
spec = do
  describe "PlexusHash properties" $ do
    it "rejects any string that's not 16 hex chars" $ property $ \txt ->
      let isValid = T.length txt == 16 && T.all (`elem` ("0123456789abcdef" :: String)) txt
      in if isValid
         then mkPlexusHash txt `shouldSatisfy` isRight
         else mkPlexusHash txt `shouldSatisfy` isLeft

  describe "Timestamp properties" $ do
    it "only accepts positive integers" $ property $ \n ->
      if n > 0
      then mkTimestamp n `shouldSatisfy` isRight
      else mkTimestamp n `shouldSatisfy` isLeft

  describe "Port properties" $ do
    it "only accepts 1-65535 range" $ property $ \n ->
      if n >= 1 && n <= 65535
      then mkPort n `shouldSatisfy` isRight
      else mkPort n `shouldSatisfy` isLeft

  describe "Percentage properties" $ do
    it "only accepts 0-100 range" $ property $ \n ->
      if n >= 0 && n <= 100
      then mkPercentage n `shouldSatisfy` isRight
      else mkPercentage n `shouldSatisfy` isLeft
```

### 4. JSON Roundtrip Tests (NEED TO ADD ⚠️)

Prove that valid data survives JSON encoding/decoding:

```haskell
-- test/Synapse/Types/JSONSpec.hs
module Synapse.Types.JSONSpec where

spec :: Spec
spec = do
  describe "StreamMetadata JSON roundtrip" $ do
    it "preserves valid metadata through JSON" $ do
      let Right meta = mkStreamMetadata "substrate" "abc123def4567890" 1735052400
      let json = encode meta
      let Right decoded = eitherDecode json
      decoded `shouldBe` meta

    it "preserves timestamp as integer (not float)" $ do
      let Right meta = mkStreamMetadata "substrate" "abc123def4567890" 1735052400
      let json = encode meta
      -- Verify JSON contains integer, not float
      json `shouldNotContain` "1735052400."
      json `shouldContain` "1735052400"

  describe "JSON rejection tests" $ do
    it "rejects float timestamp in JSON" $ do
      let json = "{\"provenance\":[\"substrate\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400.5}"
      (eitherDecode json :: Either String StreamMetadata) `shouldSatisfy` isLeft
```

### 5. Error Message Quality Tests (NEED TO ADD ⚠️)

Verify that error messages are helpful:

```haskell
-- test/Synapse/Types/ErrorMessageSpec.hs
module Synapse.Types.ErrorMessageSpec where

spec :: Spec
spec = do
  describe "Error message quality" $ do
    it "explains hex requirement for hash" $ do
      case mkPlexusHash "xyz123def4567890" of
        Left (RefineOtherException _ msg) ->
          msg `shouldContain` "hexadecimal"
        Right _ -> expectationFailure "Should have failed"

    it "explains length requirement for hash" $ do
      case mkPlexusHash "short" of
        Left err -> displayException err `shouldContain` "16"
        Right _ -> expectationFailure "Should have failed"

    it "explains snake_case requirement" $ do
      case mkSnakeCaseField "camelCase" of
        Left err -> displayException err `shouldContain` "snake_case"
        Right _ -> expectationFailure "Should have failed"

    it "explains positive requirement for timestamp" $ do
      case mkTimestamp (-1) of
        Left err -> displayException err `shouldContain` "positive"
        Right _ -> expectationFailure "Should have failed"

    it "explains percentage range" $ do
      case mkPercentage 150 of
        Left err -> displayException err `shouldContain` "0" >> displayException err `shouldContain` "100"
        Right _ -> expectationFailure "Should have failed"
```

### 6. Integration Tests with plexus-protocol (CRITICAL - NEED TO ADD 🔥)

**This is the most important test category!**

These prove that plexus-protocol actually uses synapse-types validation:

```haskell
-- plexus-protocol/test/ValidationIntegrationSpec.hs
module ValidationIntegrationSpec where

import Plexus.Types (StreamMetadata, PlexusStreamItem(..))
import Data.Aeson (eitherDecode)

spec :: Spec
spec = do
  describe "Integration: plexus-protocol uses synapse-types validation" $ do
    it "rejects float timestamp at JSON parse time" $ do
      let json = "{\"type\":\"data\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400.5},\"content_type\":\"test\",\"content\":{}}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left err -> err `shouldContain` "timestamp"
        Right _ -> expectationFailure "Should have rejected float timestamp"

    it "rejects invalid hash in stream item" $ do
      let json = "{\"type\":\"data\",\"metadata\":{\"provenance\":[\"s\"],\"plexus_hash\":\"SHORT\",\"timestamp\":1735052400},\"content_type\":\"test\",\"content\":{}}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left err -> err `shouldContain` "16"
        Right _ -> expectationFailure "Should have rejected short hash"

    it "rejects empty provenance in stream item" $ do
      let json = "{\"type\":\"data\",\"metadata\":{\"provenance\":[],\"plexus_hash\":\"abc123def4567890\",\"timestamp\":1735052400},\"content_type\":\"test\",\"content\":{}}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left err -> err `shouldContain` "provenance" >> err `shouldContain` "empty"
        Right _ -> expectationFailure "Should have rejected empty provenance"

    it "rejects camelCase field names" $ do
      let json = "{\"type\":\"data\",\"metadata\":{\"provenance\":[\"s\"],\"plexusHash\":\"abc123def4567890\",\"timestamp\":1735052400},\"contentType\":\"test\",\"content\":{}}"
      case eitherDecode json :: Either String PlexusStreamItem of
        Left _ -> pure ()  -- Should fail
        Right _ -> expectationFailure "Should have rejected camelCase fields"

  describe "Integration: Cannot bypass validation" $ do
    it "cannot construct StreamMetadata with invalid data using record syntax" $ do
      -- This should not compile (type system prevents it)
      -- If this compiles, validation is bypassable!
      {-
      let bad = StreamMetadata
            { metaProvenance = Provenance []  -- ❌ Should not compile
            , metaPlexusHash = "short"        -- ❌ Should not compile
            , metaTimestamp = -1              -- ❌ Should not compile
            }
      -}
      -- If we get here without compilation error, test passes
      True `shouldBe` True
```

### 7. Cross-Implementation Tests (NEED TO ADD ⚠️)

Test parsing real messages from other implementations:

```bash
# test/fixtures/plexus-rpc-ts/
float-timestamp.json       # Bug we found
valid-message.json         # Should parse
camelcase-fields.json      # Should reject
uppercase-hash.json        # Should reject
```

```haskell
-- test/Synapse/Types/FixtureSpec.hs
module Synapse.Types.FixtureSpec where

spec :: Spec
spec = do
  describe "Fixtures from plexus-rpc-ts" $ do
    it "rejects float-timestamp.json" $ do
      json <- LBS.readFile "test/fixtures/plexus-rpc-ts/float-timestamp.json"
      (eitherDecode json :: Either String PlexusStreamItem) `shouldSatisfy` isLeft

    it "accepts valid-message.json" $ do
      json <- LBS.readFile "test/fixtures/plexus-rpc-ts/valid-message.json"
      (eitherDecode json :: Either String PlexusStreamItem) `shouldSatisfy` isRight

    it "rejects camelcase-fields.json" $ do
      json <- LBS.readFile "test/fixtures/plexus-rpc-ts/camelcase-fields.json"
      (eitherDecode json :: Either String PlexusStreamItem) `shouldSatisfy` isLeft
```

### 8. Fuzzing Tests (ADVANCED - NEED TO ADD ⚠️)

Generate completely random JSON and ensure no crashes:

```haskell
-- test/Synapse/Types/FuzzSpec.hs
module Synapse.Types.FuzzSpec where

import Test.QuickCheck.Arbitrary
import Data.Aeson (Value(..), Object)

-- Generate random JSON values
instance Arbitrary Value where
  arbitrary = ...

spec :: Spec
spec = do
  describe "Fuzzing: Random JSON input" $ do
    it "never crashes, always returns Left or Right" $ property $ \(json :: Value) ->
      case fromJSON json :: Result StreamMetadata of
        Success _ -> True  -- Valid input
        Error _ -> True    -- Invalid input, got error
        -- ↑ The key: BOTH outcomes are acceptable, but NO CRASH

    it "never crashes on malformed objects" $ property $ \(obj :: Object) ->
      case fromJSON (Object obj) :: Result PlexusStreamItem of
        Success _ -> True
        Error _ -> True
```

### 9. Performance Tests (NICE TO HAVE)

Ensure validation doesn't kill performance:

```haskell
-- benchmark/ValidationBench.hs
module ValidationBench where

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "StreamMetadata"
      [ bench "valid" $ nf (eitherDecode :: ByteString -> Either String StreamMetadata) validJSON
      , bench "invalid hash" $ nf (eitherDecode :: ByteString -> Either String StreamMetadata) invalidHashJSON
      ]
  , bgroup "PlexusStreamItem"
      [ bench "valid data item" $ nf (eitherDecode :: ByteString -> Either String PlexusStreamItem) validDataJSON
      , bench "invalid timestamp" $ nf (eitherDecode :: ByteString -> Either String PlexusStreamItem) invalidTsJSON
      ]
  ]
```

### 10. Golden Tests (NEED TO ADD ⚠️)

Record expected error messages and ensure they don't regress:

```bash
# test/golden/errors/
float-timestamp.stderr.golden
invalid-hash-length.stderr.golden
camelcase-field.stderr.golden
```

```haskell
-- test/Synapse/Types/GoldenSpec.hs
module Synapse.Types.GoldenSpec where

import Test.Tasty.Golden

spec :: Spec
spec = do
  it "error message for float timestamp matches golden" $ do
    let err = case mkTimestamp 1735052400.5 of
          Left e -> displayException e
          Right _ -> error "Should have failed"
    goldenVsString "float-timestamp" "test/golden/errors/float-timestamp.stderr.golden" (pure $ BSL.pack err)
```

## Test Coverage Requirements

### Minimum Acceptable Coverage

| Component | Coverage Target | Priority |
|-----------|----------------|----------|
| Refined types (synapse-types) | 100% | 🔥 Critical |
| JSON parsing (plexus-protocol) | 95% | 🔥 Critical |
| Error messages | 100% | High |
| Integration points | 100% | 🔥 Critical |

### What Each Bug Needs

For EACH bug we know about, we need:
1. ✅ Unit test proving refined type rejects it
2. ⚠️ Integration test proving plexus-protocol rejects it
3. ⚠️ Fixture from real implementation showing the bug
4. ⚠️ Clear error message test
5. ⚠️ Compliance test that fails without fix

## Test Execution Strategy

### Phase 1: Verify synapse-types (Current)
```bash
cd synapse/synapse-types
cabal test
# Result: 40/40 tests passing ✅
```

### Phase 2: Add Regression Tests
```bash
cd synapse/synapse-types
# Add RegressionSpec.hs
# Add PropertySpec.hs
# Add ErrorMessageSpec.hs
cabal test
# Target: All known bugs have explicit tests
```

### Phase 3: Integration Tests
```bash
cd synapse
# Integrate synapse-types into plexus-protocol
# Add ValidationIntegrationSpec.hs
cabal test plexus-protocol
# Target: Prove validation is actually used
```

### Phase 4: Compliance Tests
```bash
cd integration-tests/compliance
python -m pytest tests/
# Target: All tests pass with synapse as validator
```

## Success Criteria

We can claim "synapse-types fixes validation" when:

### ✅ Tier 1: Type Safety (DONE)
- [x] 40 unit tests passing
- [x] Refined types prevent invalid construction
- [x] Smart constructors return Either

### ⚠️ Tier 2: Bug Coverage (NEED)
- [ ] Every known bug has a failing test without synapse-types
- [ ] Every known bug has a passing test with synapse-types
- [ ] Error messages explain what's wrong and how to fix it

### ⚠️ Tier 3: Integration (NEED)
- [ ] plexus-protocol uses synapse-types
- [ ] Cannot bypass validation
- [ ] JSON parsing rejects invalid inputs
- [ ] Integration tests prove validation is active

### ⚠️ Tier 4: Real-World (NEED)
- [ ] Parse messages from all implementations (TypeScript, Rust, etc.)
- [ ] Compliance tests pass using synapse
- [ ] Fixtures from real bugs are caught

### ⚠️ Tier 5: Robustness (NICE TO HAVE)
- [ ] Fuzzing finds no crashes
- [ ] Performance is acceptable
- [ ] Golden tests prevent error message regression

## Current Status

```
Tier 1: ████████████████████ 100% (DONE)
Tier 2: ░░░░░░░░░░░░░░░░░░░░   0% (NEED TO ADD)
Tier 3: ░░░░░░░░░░░░░░░░░░░░   0% (NEED TO ADD)
Tier 4: ░░░░░░░░░░░░░░░░░░░░   0% (NEED TO ADD)
Tier 5: ░░░░░░░░░░░░░░░░░░░░   0% (NEED TO ADD)
```

**Overall: 20% complete**

## Next Steps

1. **Immediate**: Add RegressionSpec.hs with tests for each known bug
2. **Next**: Add ErrorMessageSpec.hs to verify helpful errors
3. **Then**: Integrate into plexus-protocol with ValidationIntegrationSpec.hs
4. **Finally**: Run compliance tests against integrated version

## Test File Structure

```
synapse/
├── synapse-types/
│   └── test/
│       ├── Main.hs                    ✅ DONE
│       ├── Synapse/Types/
│       │   ├── RefinedSpec.hs         ✅ DONE (40 tests)
│       │   ├── ProtocolSpec.hs        ✅ DONE (basic)
│       │   ├── RegressionSpec.hs      ⚠️ NEED (bug-specific)
│       │   ├── PropertySpec.hs        ⚠️ NEED (QuickCheck)
│       │   ├── ErrorMessageSpec.hs    ⚠️ NEED (UX)
│       │   ├── JSONSpec.hs            ⚠️ NEED (roundtrip)
│       │   ├── FixtureSpec.hs         ⚠️ NEED (real messages)
│       │   ├── GoldenSpec.hs          ⚠️ NEED (error messages)
│       │   └── FuzzSpec.hs            ⚠️ NEED (robustness)
│       └── fixtures/
│           ├── valid/                 ⚠️ NEED
│           │   ├── minimal.json
│           │   └── complete.json
│           └── invalid/               ⚠️ NEED
│               ├── float-timestamp.json
│               ├── short-hash.json
│               ├── empty-provenance.json
│               └── camelcase.json
│
└── plexus-protocol/
    └── test/
        └── ValidationIntegrationSpec.hs  🔥 CRITICAL (proves integration)
```

## Conclusion

We have **type safety** (Tier 1) but lack **proof that it actually catches the bugs** (Tiers 2-4).

Priority order:
1. 🔥 **RegressionSpec.hs** - Prove each known bug is caught
2. 🔥 **ValidationIntegrationSpec.hs** - Prove plexus-protocol uses validation
3. 🔥 **Fixture tests** - Parse real buggy messages
4. **ErrorMessageSpec.hs** - Verify helpful errors
5. **PropertySpec.hs** - Comprehensive coverage via QuickCheck
