# Protocol Validation

This directory contains the protocol validation framework for Synapse's client-side validation of Plexus RPC servers.

## Modules

### Validator.hs
Contains the core types for protocol violations:
- `ProtocolViolation`: Represents a violation with message, severity, location, and suggested fix
- `Severity`: Error, Warning, or Info
- `Location`: Where the violation occurred (InMessage, InStream, or InConnection)
- `ValidationConfig`: Configuration for what to validate

### StreamTracker.hs
Tracks stream subscriptions and detects protocol violations in real-time:

#### Key Features
- Tracks multiple concurrent streams by subscription ID
- Detects missing `StreamDone` messages
- Detects duplicate `StreamDone` messages
- Detects messages sent after `StreamDone`
- Accumulates violations per stream

#### Usage Example

```haskell
import Synapse.Self.Protocol.StreamTracker
import Synapse.Self.Protocol.Validator
import Data.Aeson (Value)

-- Create a new tracker
tracker <- newTracker

-- Process incoming messages
violations <- trackMessage tracker message1
violations <- trackMessage tracker message2

-- Check for incomplete streams
completionViolations <- checkCompletion tracker

-- Get all violations found so far
allViolations <- getAllViolations tracker
```

## Protocol Violations Detected

### Missing StreamDone
**Severity**: Error
**Description**: A stream subscription received data messages but never received a `StreamDone` message to properly close the stream.
**Fix**: Server must send `StreamDone` to properly close each stream.

### Duplicate StreamDone
**Severity**: Error
**Description**: A stream subscription received multiple `StreamDone` messages.
**Fix**: Server should only send `StreamDone` once per subscription.

### Messages After StreamDone
**Severity**: Error
**Description**: A stream subscription received additional messages after `StreamDone` was already sent.
**Fix**: Server should not send messages after `StreamDone`.

## Testing

Run the StreamTracker tests with:
```bash
cabal test stream-tracker-test
```

## Implementation Details

### Stream State Tracking
Each stream subscription is tracked with:
- Subscription ID (integer)
- All messages received
- Whether `StreamDone` was received
- Accumulated violations
- Optional method name

### Message Parsing
The tracker parses JSON-RPC 2.0 subscription notifications:
```json
{
  "jsonrpc": "2.0",
  "method": "subscription",
  "params": {
    "subscription": 123,
    "result": {
      "type": "data|done|progress|error|guidance|request",
      ...
    }
  }
}
```

### Violation Detection
Violations are detected incrementally as messages arrive:
1. Parse the message to extract subscription ID and type
2. Update stream state
3. Check for violations (duplicate done, messages after done, etc.)
4. Return violations immediately for real-time feedback

At the end of processing, call `checkCompletion` to detect streams that never received `StreamDone`.

## Future Enhancements

Potential additions to the validator:
- Metadata validation (plexus_hash, timestamp, provenance)
- Type checking for message content
- Subscription ID consistency checking
- Message ordering validation
- Timeout detection for hanging streams
