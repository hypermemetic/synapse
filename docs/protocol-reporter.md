# Protocol Reporter

The `Synapse.Self.Protocol.Reporter` module provides comprehensive error reporting for protocol validation violations.

## Features

### Multiple Output Formats

1. **Compiler-Style Format** (`renderViolations`)
   - Detailed, human-readable format similar to compiler errors
   - Groups violations by severity (Errors, Warnings, Info)
   - Shows location, expected vs actual values, and suggested fixes
   - Best for development and debugging

2. **Summary Format** (`renderSummary`)
   - High-level overview of validation results
   - Shows counts by severity
   - Overall pass/fail status
   - Best for quick health checks

3. **Compact Format** (`renderCompact`)
   - One-line format per violation
   - Includes severity, message, location, and key details
   - Best for logs and scripting

4. **JSON Format** (`renderJSON`)
   - Machine-readable JSON output
   - Includes all violation details and summary statistics
   - Best for tooling integration and CI/CD pipelines

## Usage

```haskell
import Synapse.Self.Protocol.Validator (validateStreamItem)
import Synapse.Self.Protocol.Reporter (renderViolations, renderSummary, renderJSON)
import qualified Data.Text.IO as TIO

-- Validate stream items
violations <- validateStreamItem item

-- Show detailed errors
TIO.putStrLn $ renderViolations violations

-- Show summary
TIO.putStrLn $ renderSummary violations

-- Output JSON for tooling
TIO.putStrLn $ renderJSON violations
```

## Example Output

### Compiler-Style Format

```
X error: Missing 'plexus_hash' field in metadata
  --> subscription_42:msg_3.metadata
  |
  | Expected: 16-character lowercase hex string
  | Actual: missing
  | Fix: Add plexus_hash field with schema version hash

! warning: Protocol field 'contentType' uses camelCase (should be snake_case)
  --> subscription_42:msg_2.contentType
  |
  | Expected: content_type
  | Actual: contentType
  | Fix: Rename to: content_type
```

### Summary Format

```
Protocol Validation Results:
  X 3 errors
  ! 1 warnings
  * 0 info

FAILED: Protocol violations detected
```

### Compact Format

```
[ERROR] Missing 'plexus_hash' field in metadata at subscription_42:msg_3.metadata: expected 16-character lowercase hex string, got missing
[WARNING] Protocol field 'contentType' uses camelCase (should be snake_case) at subscription_42:msg_2.contentType: expected content_type, got contentType
```

### JSON Format

```json
{
  "violations": [
    {
      "message": "Missing 'plexus_hash' field in metadata",
      "severity": "error",
      "location": {
        "type": "message",
        "subscription_id": 42,
        "message_index": 3,
        "field": "metadata"
      },
      "expected": "16-character lowercase hex string",
      "actual": "missing",
      "fix": "Add plexus_hash field with schema version hash"
    }
  ],
  "summary": {
    "errors": 1,
    "warnings": 0,
    "info": 0,
    "passed": false
  }
}
```

## Severity Symbols

- `X` - Error (critical issues that must be fixed)
- `!` - Warning (issues that should be addressed)
- `*` - Info (suggestions and best practices)

## Helper Functions

### `groupBySeverity`
Groups violations by severity level for organized display.

### `formatLocation`
Converts Location values into human-readable strings:
- `InMessage`: `subscription_42:msg_3.metadata`
- `InStream`: `stream:echo.request (after 5 messages)`
- `InConnection`: `connection`

### `severitySymbol` and `severityName`
Convert severity enums to display symbols and names.

## Testing

Run the demo to see all output formats:

```bash
cabal run reporter-demo --flag build-examples
```

## Integration with Validator

The Reporter module works seamlessly with the Validator module:

```haskell
import Synapse.Self.Protocol.Validator (validateStreamItem)
import Synapse.Self.Protocol.Reporter (renderViolations)

validateAndReport :: Value -> IO ()
validateAndReport item = do
  violations <- validateStreamItem item
  TIO.putStrLn $ renderViolations violations
```

## Design Decisions

1. **ASCII Symbols**: Uses ASCII symbols (X, !, *) instead of Unicode for maximum terminal compatibility
2. **Text Builders**: Uses `Text.Lazy.Builder` for efficient string construction
3. **Severity Ordering**: Violations are ordered by severity (Error > Warning > Info)
4. **Location Details**: Precise location tracking with subscription IDs, message indices, and field paths
5. **Actionable Output**: Every violation includes expected/actual values and suggested fixes when available
