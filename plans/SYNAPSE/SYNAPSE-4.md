# SYNAPSE-4: HTTP/SSE Transport

**blocked_by**: [SYNAPSE-2]
**unlocks**: [SYNAPSE-6]

## Scope

Implement HTTP client for fetching schema and SSE client for streaming method invocations. The substrate exposes JSON-RPC over HTTP with SSE for streaming responses.

## Files to Create

```
synapse/src/Synapse/Transport.hs
synapse/synapse.cabal  -- add http-client, http-client-tls, http-types
```

## Dependencies to Add

```cabal
build-depends:
  , http-client      >= 0.7
  , http-client-tls  >= 0.3
  , http-types       >= 0.12
  , conduit          >= 1.3
  , conduit-extra    >= 1.3
```

## Core Types

```haskell
-- | Transport configuration
data TransportConfig = TransportConfig
  { tcBaseUrl :: !Text       -- e.g., "http://localhost:3000"
  , tcTimeout :: !Int        -- seconds
  }

-- | Transport errors
data TransportError
  = HttpError Text
  | ParseError Text
  | TimeoutError
  deriving (Show, Eq)
```

## Core Functions

### Schema Fetching

```haskell
-- | Fetch the full plugin schema
fetchSchema :: TransportConfig -> IO (Either TransportError PluginSchema)
fetchSchema cfg = do
  -- POST to /rpc with {"method": "plexus.schema", "params": {}}
  -- Parse response as PluginSchema
  ...

-- | Fetch just the root hash (for cache invalidation)
fetchHash :: TransportConfig -> IO (Either TransportError Text)
fetchHash cfg = do
  -- POST to /rpc with {"method": "plexus.hash", "params": {}}
  ...
```

### Method Invocation (SSE)

```haskell
-- | Invoke a method and stream results
invoke
  :: TransportConfig
  -> Text           -- method name (e.g., "echo.echo")
  -> Value          -- params
  -> ConduitT () StreamItem IO ()
invoke cfg method params = do
  -- POST to /rpc with {"method": method, "params": params}
  -- Parse SSE stream as StreamItem values
  ...
```

## Wire Protocol

### JSON-RPC Request
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "echo.echo",
  "params": {"message": "hello", "count": 3}
}
```

### SSE Response Stream
```
data: {"type":"data","kind":"echo","payload":"hello","request_id":"...","timestamp":"..."}

data: {"type":"data","kind":"echo","payload":"hello","request_id":"...","timestamp":"..."}

data: {"type":"done","request_id":"...","timestamp":"..."}
```

## Acceptance Criteria

1. `fetchSchema` successfully parses substrate schema with hashes
2. `fetchHash` returns root hash matching schema.hash
3. `invoke` streams items in real-time (not buffered)
4. Timeout handling works correctly
5. Connection errors produce clear error messages
