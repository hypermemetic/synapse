# Ticket 2.5: Protocol Test Runner - Quick Start Guide

## What Was Implemented

A comprehensive test runner that validates Plexus RPC protocol compliance by testing debug endpoints and reporting violations in a compiler-style format.

## Files

```
synapse/
├── src/Synapse/Self/Protocol/
│   └── TestRunner.hs              # Core test runner (363 lines)
├── test/
│   └── ProtocolTestDemo.hs        # Demo executable
├── docs/
│   └── PROTOCOL_TEST_RUNNER.md    # Full documentation
└── plexus-synapse.cabal           # Updated with new module
```

## Quick Usage

### 1. Build the Project

```bash
cd /workspace/hypermemetic/synapse
cabal build synapse
```

### 2. Run the Demo

```bash
# Build the demo (requires flag)
cabal build protocol-test-demo --flag build-examples

# Test a backend
cabal run protocol-test-demo --flag build-examples -- localhost 3000 bash
```

### 3. Use in Your Code

```haskell
import Synapse.Self.Protocol.TestRunner

main :: IO ()
main = do
  result <- runProtocolTests "localhost" 3000 "bash"
  case result of
    Left report -> putStrLn $ "FAILED:\n" <> T.unpack report
    Right report -> putStrLn $ "PASSED:\n" <> T.unpack report
```

## What It Tests

The runner executes 5 test scenarios:

1. **Protocol Test** - Basic protocol compliance
2. **Stream Test (slow)** - Slow streaming behavior
3. **Stream Test (progress)** - Progress message handling
4. **Error Test (recoverable)** - Recoverable error handling
5. **Metadata Test** - Metadata edge cases

Each test validates:
- Stream lifecycle (StreamDone detection)
- Message structure (type, fields)
- Metadata compliance (provenance, hash, timestamp)
- Type-specific requirements (data, progress, error, done)

## Example Output

### Success
```
Protocol Validation Results:
  X 0 errors
  ! 0 warnings
  * 0 info

PASSED: No protocol violations

Test Results:
  [Protocol Test] PASS (5 messages)
  [Stream Test (slow)] PASS (3 messages)
  [Stream Test (progress)] PASS (4 messages)
  [Error Test (recoverable)] PASS (2 messages)
  [Metadata Test] PASS (3 messages)
```

### Failure
```
Protocol Violations:
----------------------------------------

Errors:
----------------------------------------

X error: Missing StreamDone message
  --> stream:_debug.protocol_test (after 5 messages)
  |
  | Expected: StreamDone message to complete stream
  | Actual: 5 messages without StreamDone
  | Fix: Server must send StreamDone to properly close each stream

...

FAILED: Protocol violations detected
```

## Architecture

```
runProtocolTests (main entry point)
    ↓
runTestSuite (5 tests)
    ↓
testEndpoint (per-endpoint testing)
    ↓
    ├─→ StreamTracker (lifecycle tracking)
    ├─→ Validator (structure validation)
    └─→ Reporter (violation formatting)
    ↓
Aggregated report (Either Text Text)
```

## Key Functions

### runProtocolTests
```haskell
runProtocolTests :: Text -> Int -> Text -> IO (Either Text Text)
```
- Main entry point
- Runs all 5 tests
- Returns comprehensive report

### testEndpoint
```haskell
testEndpoint :: SubstrateConfig -> Text -> Value -> IO (Either Text ([ProtocolViolation], Int))
```
- Tests single endpoint
- Returns violations and message count
- 15-second timeout per test

## Dependencies

The test runner integrates with:

- **StreamTracker** (Ticket 2.2) - Tracks stream lifecycle
- **Validator** (Ticket 2.3) - Validates message structure
- **Reporter** (Ticket 2.4) - Formats violations
- **Plexus.Transport** - WebSocket communication
- **Plexus.Types** - Stream item types

## Configuration

**Timeout:** 15 seconds per endpoint (configurable in code)

**Test Endpoints:** Modify `runTestSuite` function to add/change tests

**Backend Connection:** Uses standard SubstrateConfig

## Troubleshooting

### Build Issues

```bash
# Clean build
cabal clean
cabal build synapse

# Check dependencies
cabal build --dependencies-only
```

### Runtime Issues

**Connection refused:**
- Check backend is running
- Verify host/port are correct
- Check firewall settings

**Timeout:**
- Backend may be slow
- Increase timeout in code
- Check network connection

**Protocol violations:**
- Review violation report
- Check backend implementation
- Verify debug endpoints exist

## Next Steps

1. Integrate into CI/CD pipeline
2. Add custom validation rules
3. Implement parallel testing
4. Add performance metrics
5. Create continuous monitoring mode

## References

- Full documentation: `/workspace/hypermemetic/synapse/docs/PROTOCOL_TEST_RUNNER.md`
- Implementation summary: `/workspace/hypermemetic/synapse/TICKET_2.5_SUMMARY.md`
- Source code: `/workspace/hypermemetic/synapse/src/Synapse/Self/Protocol/TestRunner.hs`
