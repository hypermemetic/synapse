# Validation Gaps: Synapse as Reference Implementation

**Date**: 2025-03-20
**Status**: Critical gaps identified
**Related**: synapse-types package, compliance testing

## Executive Summary

Synapse currently **CANNOT** serve as a reliable validation mechanism for Plexus RPC implementations. The current types in `plexus-protocol` have NO runtime validation and will silently accept malformed protocol messages.

### Critical Answer to Your Questions

1. **Can it catch pathological cases?** ❌ NO
2. **Can it detect non-conformity cleanly?** ❌ NO
3. **Have you already implemented the types into synapse?** ❌ NO

## Current State Analysis

### What Synapse Currently Uses

From `plexus-protocol/src/Plexus/Types.hs`:

```haskell
-- Line 156-160: StreamMetadata with NO validation
data StreamMetadata = StreamMetadata
  { metaProvenance :: Provenance      -- Plain list (can be empty!)
  , metaPlexusHash :: Text             -- Any text (no length/format check!)
  , metaTimestamp  :: Integer          -- ⚠️ ACCEPTS FLOATS (truncates!)
  }

-- Line 163-167: FromJSON just parses directly
instance FromJSON StreamMetadata where
  parseJSON = withObject "StreamMetadata" $ \o -> StreamMetadata
    <$> o .: "provenance"
    <*> o .: "plexus_hash"        -- No validation!
    <*> o .: "timestamp"          -- Silently truncates floats!
```

### Critical Bugs Synapse WON'T Catch

#### 1. Float Timestamp Bug (THE MAJOR ISSUE)

**Problem**: `metaTimestamp :: Integer` in Aeson will:
- Accept JSON `1735052400.5`
- Silently truncate to `1735052400`
- **NO ERROR, NO WARNING**

**Example**:
```json
{
  "provenance": ["substrate"],
  "plexus_hash": "abc123def4567890",
  "timestamp": 1735052400.789   ← SILENTLY TRUNCATED!
}
```

This is EXACTLY the bug we found in plexus-rpc-ts! Synapse would accept it without complaint.

#### 2. Invalid Hash Bug

**Problem**: `metaPlexusHash :: Text` accepts ANY string:

```json
{
  "plexus_hash": "not-hex-at-all-way-too-long-123456789"
}
```

✅ Parsed successfully
❌ Should reject: wrong length, non-hex characters

#### 3. Empty Provenance Bug

**Problem**: `Provenance = Provenance { segments :: [Text] }` - plain list!

```json
{
  "provenance": []   ← EMPTY! Protocol requires non-empty!
}
```

✅ Parsed successfully
❌ Should reject: provenance must be non-empty

#### 4. Invalid Percentage Bug

**Problem**: `itemPercentage :: Maybe Double` - no bounds checking:

```json
{
  "type": "progress",
  "percentage": 150   ← OUT OF RANGE!
}
```

✅ Parsed successfully
❌ Should reject: percentage must be 0-100

#### 5. camelCase Field Names

**Problem**: Parser uses `o .: "plexus_hash"` but gives generic error:

```json
{
  "plexusHash": "abc123def4567890"   ← WRONG CASE!
}
```

❌ Parse error: "key 'plexus_hash' not found"
😞 Error message doesn't explain the actual problem (camelCase vs snake_case)

## What We Created But Haven't Integrated

The `synapse-types` package has ALL the validation we need:

```haskell
-- synapse-types/src/Synapse/Types/Protocol.hs
data StreamMetadata = StreamMetadata
  { smProvenance :: !Provenance      -- NonEmpty Text (can't be empty!)
  , smPlexusHash :: !PlexusHash      -- Refined (SizeEqualTo 16 `And` Hex) Text
  , smTimestamp  :: !Timestamp       -- Refined Positive Int64 (rejects floats!)
  }

mkStreamMetadata
  :: Text -> Text -> Int64
  -> Either RefineException StreamMetadata
```

**This would catch all the bugs!** But it's not integrated.

## Integration Gap

```
┌─────────────────────────────────────┐
│ synapse-types (NEW)                 │
│ ✅ Refined types with validation    │
│ ✅ 40 passing tests                 │
│ ✅ Type-level guarantees            │
└─────────────────────────────────────┘
         │
         │ NOT INTEGRATED!
         ↓
┌─────────────────────────────────────┐
│ plexus-protocol (CURRENT)           │
│ ❌ No validation                    │
│ ❌ Accepts malformed data           │
│ ❌ Silent truncation                │
└─────────────────────────────────────┘
         │
         │ Used by
         ↓
┌─────────────────────────────────────┐
│ synapse                              │
│ ❌ Can't validate other impls       │
│ ❌ Can't detect non-conformity      │
│ ❌ Poor error messages              │
└─────────────────────────────────────┘
```

## Error Message Quality

### Current Behavior

When parsing fails, Aeson gives generic errors:

```
Error in $.metadata: key "plexus_hash" not present
```

User has NO IDEA that:
- They might have used camelCase
- The field name must be snake_case
- This is a wire format requirement

### With Refined Types

```haskell
Left (RefineOtherException "Must be exactly 16 hexadecimal characters")
Left (RefineOtherException "Must be snake_case (lowercase letters, digits, underscores)")
Left (RefineOtherException "Timestamp must be positive integer (got float)")
```

Clear, actionable error messages!

## What Synapse Needs to Be a Validator

### 1. Replace plexus-protocol types with synapse-types

```haskell
-- In plexus-protocol/src/Plexus/Types.hs
import qualified Synapse.Types.Protocol as Validated
import qualified Synapse.Types.WireFormat as Validated

-- Use validated types for parsing
instance FromJSON StreamMetadata where
  parseJSON = withObject "StreamMetadata" $ \o -> do
    serverName <- o .: "provenance" >>= \case
      (x:_) -> pure x
      [] -> fail "Provenance cannot be empty"
    hashText <- o .: "plexus_hash"
    tsValue <- o .: "timestamp"

    -- Validate with refined types
    case Validated.mkStreamMetadata serverName hashText tsValue of
      Right validated -> pure $ toPlexusMetadata validated
      Left err -> fail $ show err
```

### 2. Add compliance test mode

```haskell
-- In synapse executable
validateMessage :: ByteString -> IO (Either ValidationError ())
validateMessage json = do
  case eitherDecode json of
    Left parseErr -> return $ Left $ ParseError parseErr
    Right (item :: PlexusStreamItem) ->
      case validateStreamItem item of
        [] -> return $ Right ()
        errs -> return $ Left $ ValidationErrors errs
```

### 3. Create validation CLI

```bash
# Validate a message
$ synapse validate message.json
✗ Invalid plexus_hash: must be exactly 16 hex characters (got 12)
✗ Invalid timestamp: must be positive integer (got float: 1735052400.5)

# Validate a stream
$ synapse validate-stream < stream.jsonl
Message 1: ✓
Message 2: ✗ Empty provenance (line 23)
Message 3: ✓
```

## Detection Capabilities Comparison

| Issue | Current Synapse | With synapse-types Integration |
|-------|----------------|-------------------------------|
| Float timestamps | ❌ Silent truncation | ✅ Rejected with clear error |
| Invalid hash format | ❌ Accepts any text | ✅ Rejected with format requirements |
| Invalid hash length | ❌ Accepts any length | ✅ Rejected (must be exactly 16) |
| Empty provenance | ❌ Accepts empty list | ✅ Rejected (must be non-empty) |
| Percentage out of range | ❌ Accepts any number | ✅ Rejected (must be 0-100) |
| Negative timeout | ❌ Accepts negative | ✅ Rejected (must be positive) |
| camelCase fields | ⚠️ Parse error (cryptic) | ✅ Clear message about snake_case |
| Missing metadata | ⚠️ Parse error (cryptic) | ✅ Type prevents construction |

## Pathological Cases

Here are examples of pathological messages synapse should detect:

### Case 1: Malicious Hash Injection
```json
{
  "plexus_hash": "'; DROP TABLE users; --",
  "timestamp": 1735052400,
  "provenance": ["evil"]
}
```
**Current**: ✅ Accepts
**Should**: ❌ Reject (not hex)

### Case 2: Time Precision Loss
```json
{
  "timestamp": 1735052400.999999,
  "plexus_hash": "abc123def4567890",
  "provenance": ["substrate"]
}
```
**Current**: ✅ Accepts (truncates to 1735052400)
**Should**: ❌ Reject (must be integer)

### Case 3: Empty Call Chain
```json
{
  "provenance": [],
  "plexus_hash": "abc123def4567890",
  "timestamp": 1735052400
}
```
**Current**: ✅ Accepts
**Should**: ❌ Reject (must be non-empty)

### Case 4: Buffer Overflow Attempt
```json
{
  "plexus_hash": "a".repeat(1000000),
  "timestamp": 1735052400,
  "provenance": ["substrate"]
}
```
**Current**: ✅ Accepts (memory allocation)
**Should**: ❌ Reject (must be exactly 16 chars)

## Recommendations

### Immediate (High Priority)

1. **Integrate synapse-types into plexus-protocol**
   - Replace unvalidated types with refined types
   - Add validation in FromJSON instances
   - Preserve backward compatibility with conversion functions

2. **Add validation CLI to synapse**
   - `synapse validate` command
   - Returns exit code 0 for valid, 1 for invalid
   - Machine-readable output (JSON)

3. **Update compliance tests to use synapse**
   - Run compliance tests against synapse validation
   - Ensure synapse catches all known bugs

### Medium Priority

4. **Create validation server**
   - HTTP endpoint: POST /validate
   - Returns structured validation errors
   - Used by CI/CD pipelines

5. **Add fuzzing tests**
   - Generate random malformed messages
   - Ensure synapse rejects all invalid inputs
   - No crashes, clear errors

### Long Term

6. **Make synapse the compliance oracle**
   - All Plexus RPC implementations test against synapse
   - Synapse defines "correct behavior"
   - Automated compliance badges

## Conclusion

**Current State**: Synapse is NOT a reliable validator. It has the same bugs as the implementations it should be validating.

**With Integration**: Synapse could be THE reference implementation, catching all protocol violations with clear, actionable errors.

**The types exist** (synapse-types with 40 passing tests), but **integration is required** to make synapse a trustworthy validation mechanism.
