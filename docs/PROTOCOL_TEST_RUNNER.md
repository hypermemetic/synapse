# Protocol Test Runner

## Overview

The Protocol Test Runner (`Synapse.Self.Protocol.TestRunner`) orchestrates comprehensive protocol validation tests for Plexus RPC backends. It coordinates the StreamTracker, Validator, and Reporter components to provide end-to-end protocol compliance testing.

## Implementation: Ticket 2.5

**Location:** `/workspace/hypermemetic/synapse/src/Synapse/Self/Protocol/TestRunner.hs`

### Key Components

#### 1. Main Test Runner

```haskell
runProtocolTests :: Text -> Int -> Text -> IO (Either Text Text)
```

**Purpose:** Runs a complete suite of protocol validation tests against a backend.

**Parameters:**
- `host` - Target host (e.g., "localhost")
- `port` - Port number (e.g., 3000)
- `backend` - Backend name (e.g., "bash")

**Returns:**
- `Left errorReport` - If tests fail or protocol violations are detected
- `Right successReport` - If all tests pass

**Test Suite:**
1. **Protocol Test** - Basic protocol compliance via `_debug.protocol_test`
2. **Stream Test (slow)** - Tests slow streaming scenario
3. **Stream Test (progress)** - Tests progress message handling
4. **Error Test (recoverable)** - Tests error handling with recoverable errors
5. **Metadata Test** - Tests metadata edge cases

#### 2. Helper Function: testEndpoint

```haskell
testEndpoint :: SubstrateConfig -> Text -> Value -> IO (Either Text ([ProtocolViolation], Int))
```

**Purpose:** Tests a single endpoint for protocol violations.

**How it works:**
1. Creates a StreamTracker instance
2. Connects to the endpoint via WebSocket
3. Subscribes to the stream using `{backend}.call`
4. For each message received:
   - Converts PlexusStreamItem to Value
   - Tracks message via StreamTracker (detects duplicate StreamDone, etc.)
   - Validates message structure via Validator
   - Collects all violations
5. After stream completes:
   - Checks for completion violations (missing StreamDone)
   - Returns all violations and message count
6. Includes 15-second timeout per endpoint

**Error Handling:**
- Connection failures → Transport error message
- Timeouts → Timeout error message
- Protocol violations → Detailed violation report

#### 3. Type Conversions

The `plexusItemToValue` function converts between `PlexusStreamItem` (Haskell type) and `Value` (JSON) for validation:

```haskell
plexusItemToValue :: PT.PlexusStreamItem -> Value
```

**Conversions:**
- `StreamData` → `{"type":"data", "content_type":..., "content":..., "metadata":{...}}`
- `StreamProgress` → `{"type":"progress", "message":..., "percentage":..., "metadata":{...}}`
- `StreamError` → `{"type":"error", "message":..., "code":..., "metadata":{...}}`
- `StreamDone` → `{"type":"done", "metadata":{...}}`
- `StreamRequest` → `{"type":"request", "request_id":..., "request_data":..., ...}`
- `StreamGuidance` → `{"type":"guidance", "error_type":..., "suggestion":..., ...}`

Each item includes metadata with:
- `provenance` - Array of server names
- `plexus_hash` - 16-character hex hash
- `timestamp` - Unix epoch (set to 0 for testing)

### Test Flow

```
runProtocolTests
    ↓
runTestSuite (runs 5 tests in sequence)
    ↓
testEndpointWithName (wraps testEndpoint with test name)
    ↓
testEndpoint (core testing logic)
    ↓
    ├─→ Create StreamTracker
    ├─→ Connect via PT.rpcCallStreaming
    ├─→ For each message:
    │   ├─→ plexusItemToValue (convert to JSON)
    │   ├─→ trackMessage (stream-level validation)
    │   ├─→ validateStreamItem (structure validation)
    │   └─→ Collect violations
    ├─→ Wait for completion (15s timeout)
    └─→ getAllViolations (check for incomplete streams)
    ↓
Aggregate violations from all tests
    ↓
Generate report using Reporter
    ↓
Return Either Text Text (error report or success)
```

### Integration with Other Components

**StreamTracker Integration:**
- Creates one tracker per test endpoint
- Calls `trackMessage` for each received message
- Calls `getAllViolations` at end to check for completion violations
- Detects: duplicate StreamDone, messages after StreamDone, missing StreamDone

**Validator Integration:**
- Calls `validateStreamItem` for each message
- Validates: message structure, metadata, field naming, type-specific fields
- Returns structured violation list

**Reporter Integration:**
- Calls `renderViolations` to format detailed violation reports
- Calls `renderSummary` to show test summary
- Formats test results with pass/fail status

**Transport Integration:**
- Uses `Plexus.Transport.rpcCallStreaming` for async message handling
- Connects via `SubstrateConfig` (host, port, backend)
- Calls `{backend}.call` with method and params
- Handles transport errors gracefully

### Usage Examples

#### Command Line Demo

```bash
# Build the demo executable
cabal build protocol-test-demo --flag build-examples

# Run against local backend
cabal run protocol-test-demo -- localhost 3000 bash

# Run against remote backend
cabal run protocol-test-demo -- example.com 8080 python
```

#### Programmatic Usage

```haskell
import Synapse.Self.Protocol.TestRunner

main :: IO ()
main = do
  result <- runProtocolTests "localhost" 3000 "bash"
  case result of
    Left errorReport -> do
      putStrLn "Protocol violations detected:"
      putStrLn (T.unpack errorReport)
    Right successReport -> do
      putStrLn "All tests passed!"
      putStrLn (T.unpack successReport)
```

### Output Format

#### Success Report

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

#### Error Report

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


Protocol Validation Results:
  X 1 errors
  ! 0 warnings
  * 0 info

FAILED: Protocol violations detected

Test Results:
  [Protocol Test] FAIL (1 violations) (5 messages)
  [Stream Test (slow)] PASS (3 messages)
  [Stream Test (progress)] PASS (4 messages)
  [Error Test (recoverable)] PASS (2 messages)
  [Metadata Test] PASS (3 messages)
```

### Configuration

**Timeout:**
- Default: 15 seconds per endpoint
- Configurable via the `timeout` function parameter (in microseconds)
- Change in code: `timeout 15000000` → `timeout <your-value>`

**Test Endpoints:**
- Modify `runTestSuite` to add/remove/change tests
- Each test calls a specific `_debug.*` endpoint
- Tests run sequentially (not in parallel)

**Backend Connection:**
- Uses `SubstrateConfig` from plexus-protocol
- Connects via `{backend}.call` RPC method
- All tests use the same backend connection

### Error Handling

**Transport Errors:**
- Connection refused → User-friendly error message
- Connection timeout → Timeout error with host/port
- Network errors → Network error description
- Protocol errors → Protocol error details

**Test Execution Errors:**
- Captured in `TestResult.trError`
- Displayed separately from protocol violations
- Does not prevent other tests from running

**Timeout Handling:**
- Each test has independent 15s timeout
- Timeout prevents hanging on slow/broken endpoints
- Returns clear timeout error message

### Testing the Test Runner

The test runner itself can be tested using a compliant backend with debug endpoints:

```bash
# Assuming you have a backend with _debug endpoints running
cabal run protocol-test-demo -- localhost 3000 your-backend
```

Expected behavior:
- All 5 tests should execute
- Each test should receive messages and complete
- No protocol violations if backend is compliant
- Clear error messages if backend has issues

### Design Decisions

1. **Sequential Testing:** Tests run one at a time to avoid overwhelming the backend and to make debugging easier.

2. **Timeout per Test:** Each test has its own timeout to prevent one slow test from blocking others.

3. **Comprehensive Violation Tracking:** Combines stream-level violations (StreamTracker) with structure violations (Validator) for complete coverage.

4. **Graceful Error Handling:** Connection errors don't crash the test runner; they're captured and reported.

5. **Detailed Reporting:** Uses Reporter for consistent, compiler-style error formatting.

6. **Type Safety:** Converts PlexusStreamItem to Value for validation while maintaining type safety in the conversion.

### Future Enhancements

Potential improvements for future tickets:

- **Parallel Testing:** Run tests concurrently for faster execution
- **Configurable Test Suite:** Load test definitions from YAML/JSON
- **Performance Metrics:** Track message rate, latency, etc.
- **Custom Assertions:** Allow user-defined validation rules
- **Test Coverage Analysis:** Report which parts of protocol were tested
- **Continuous Monitoring:** Long-running test mode for stability testing

## Related Modules

- **Synapse.Self.Protocol.Validator** - Message structure validation
- **Synapse.Self.Protocol.StreamTracker** - Stream lifecycle tracking
- **Synapse.Self.Protocol.Reporter** - Violation formatting and reporting
- **Plexus.Transport** - WebSocket transport layer
- **Plexus.Types** - Stream item type definitions
