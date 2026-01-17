# Synapse Reimplementation Plan

**Date**: 2026-01-16
**Status**: In Progress
**Branch**: `feature/synapse-reimplementation`
**Base Commit**: `6defae7` (Jan 6 revert point)

## Overview

After reverting to the Jan 6 state, we need to cleanly reimplement several features that were added but either incomplete or over-engineered. This document captures:

1. Features to reimplement from the Jan 16 work
2. New backend discovery abstraction
3. Implementation approach for each

---

## 1. Splash Screen for Bare `synapse` Command

**Status**: ✅ Implemented

The bare `synapse` command now shows:
- ASCII splash
- synapse-level optparse help
- Available backends (as subcommands)

**Files Changed**:
- `app/Main.hs`: Added `NoCmd` variant, `topLevelHeader`, updated `main`

---

## 2. Backend Discovery Abstraction

**Status**: 🔲 To Implement

### Problem

Currently, backends are hardcoded as optparse subcommands. We want to support dynamic backend discovery while keeping the current behavior as a stub.

### Future Vision

```
┌─────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   synapse   │────▶│  Hub Directory   │────▶│  Backend Hubs   │
│   (CLI)     │     │  (port 4444)     │     │  (discovered)   │
└─────────────┘     └──────────────────┘     └─────────────────┘
                            │
                            ▼
                    Returns list of:
                    - Hub name
                    - Connection info (host:port or https URL)
                    - Description
                    - Schema hash
```

### Abstraction Layer

```haskell
-- | A discovered backend
data Backend = Backend
  { backendName        :: Text           -- ^ e.g., "plexus"
  , backendDescription :: Text           -- ^ Human-readable description
  , backendHost        :: Text           -- ^ Host to connect to
  , backendPort        :: Int            -- ^ Port
  , backendVersion     :: Maybe Text     -- ^ Version if known
  }

-- | Backend discovery interface (will be a typeclass or record-of-functions)
data BackendDiscovery = BackendDiscovery
  { discoverBackends :: IO [Backend]
      -- ^ Query for available backends
  , getBackendInfo   :: Text -> IO (Maybe Backend)
      -- ^ Get info for a specific backend by name
  }

-- | Current stub: hardcoded plexus on localhost:4444
stubDiscovery :: BackendDiscovery
stubDiscovery = BackendDiscovery
  { discoverBackends = pure
      [ Backend "plexus" "Plexus Hub" "127.0.0.1" 4444 Nothing ]
  , getBackendInfo = \name ->
      if name == "plexus"
        then pure $ Just $ Backend "plexus" "Plexus Hub" "127.0.0.1" 4444 Nothing
        else pure Nothing
  }

-- | Future: query a directory service
directoryDiscovery :: Text -> Int -> BackendDiscovery
directoryDiscovery dirHost dirPort = BackendDiscovery
  { discoverBackends = queryDirectory dirHost dirPort
  , getBackendInfo   = queryBackend dirHost dirPort
  }
```

### CLI Integration

```haskell
main :: IO ()
main = do
  -- Use stub for now, swap for directory later
  let discovery = stubDiscovery

  backends <- discoverBackends discovery
  cmd <- execParser (argsInfo backends)

  case cmd of
    NoCmd -> do
      TIO.putStr splash
      TIO.putStr $ renderBackendList backends
    BackendCmd name args -> do
      mBackend <- getBackendInfo discovery name
      case mBackend of
        Nothing -> die $ "Unknown backend: " <> name
        Just backend -> runBackend backend args
```

### Display Format

When running bare `synapse`:

```
███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗
...

synapse - Multi-backend CLI

Usage: synapse <backend> [command] [options]

Available backends:
  plexus     Plexus Hub (127.0.0.1:4444)

Run 'synapse <backend>' for backend-specific options.
```

---

## 3. Dot-Notation Path Support

**Status**: 🔲 To Implement

### Feature

Allow `synapse plexus.cone.chat` alongside `synapse plexus cone chat`.

### Implementation

Minimal 3-line change in `parsePathAndParams`:

```haskell
-- In parsePathAndParams, when processing path segments:
| otherwise =
    let segments = filter (not . T.null) $ T.splitOn "." x
    in go (reverse segments ++ path) params xs
```

### Considerations

- Keep it simple (lexical split on `.`)
- No escape mechanism needed for now
- Works with existing navigation

---

## 4. CLI Parameter Transform Layer

**Status**: ✅ Implemented

### Features Implemented

1. **Path expansion**: `.` → cwd, `~/foo` → home expansion ✅
2. **Environment variable expansion**: `$VAR` → value ✅
3. **Type-aware boolean flags**: `--approve` → `--approve true` (only for boolean params per IR) ✅

### Architecture

Two-stage approach:

```
CLI args
  ↓
parsePathAndParams (with boolean flag fix)
  ↓
[(Text, Text)] raw params
  ↓
transformParams (pre-parse)     ← Path expansion, env vars
  ↓
[(Text, Text)] transformed
  ↓
injectBooleanDefaults           ← Type-aware boolean injection
  ↓
parseParams (IR-driven)
  ↓
Value (JSON)
  ↓
invokeMethod
```

### Files Changed

- `src/Synapse/CLI/Transform.hs`: Restored from reverted commit
- `hub-synapse.cabal`: Added `Synapse.CLI.Transform` to exposed modules
- `app/Main.hs`:
  - Added import for Transform module
  - Integrated `transformParams` after `parsePathAndParams`
  - Integrated `injectBooleanDefaults` before `parseParams`
  - Fixed boolean flag parsing in `parsePathAndParams` (don't consume next `--flag` as value)

### Tests Passed

- `synapse plexus loopback respond --approve --approval_id <uuid>` ✅
- `$USER` expansion in params ✅
- `--working_dir .` expands to absolute path ✅

---

## 5. KindStringEnum Parsing Fix

**Status**: ✅ Implemented (Bug Fix)

### Problem

`buildFromTypeDef` in `Parse.hs` had non-exhaustive pattern match - missing `KindStringEnum` case.

### Fix

Added handling for `KindStringEnum` to accept string values and validate against allowed values.

**File Changed**: `src/Synapse/CLI/Parse.hs`

### Test Passed

- `synapse plexus claudecode create --model haiku ...` ✅

---

## 6. Configurable Backend (substrate-protocol)

**Status**: 🔲 Deferred

### Previous Approach (Over-Engineered)

Added 3 fields to `SubstrateConfig`:
- `substrateBackend`
- `substrateCallMethod`
- `substrateSchemaMethod`

### Correct Approach

The protocol defines conventions:
- `{backend}.call` - routing method (exists on hubs)
- `{backend}.schema` - schema method (exists on everything)

If we need configurability, add only:
```haskell
substrateBackend :: Text  -- e.g., "plexus"
-- Derive: callMethod = backend <> ".call"
-- Derive: schemaMethod = backend <> ".schema"
```

### Decision

Defer until we actually have multiple backends. Current hardcoded "plexus" is fine.

---

## Implementation Order

1. ✅ Splash screen (done)
2. 🔲 Backend discovery abstraction + stub
3. 🔲 Dot-notation paths (simple, low risk)
4. ✅ Transform layer integration (done)
5. ✅ KindStringEnum fix (done)
6. 🔲 Configurable backend (deferred)

---

## Files Changed This Session

| File | Change |
|------|--------|
| `app/Main.hs` | Splash for bare `synapse`, Transform integration, boolean flag parsing fix |
| `src/Synapse/CLI/Transform.hs` | Restored from reverted commit |
| `src/Synapse/CLI/Parse.hs` | Added `KindStringEnum` handling |
| `hub-synapse.cabal` | Added `Synapse.CLI.Transform` module |
| `docs/architecture/...` | This document |

---

## Testing Checklist

- [x] `synapse` shows splash + available backends
- [x] `synapse plexus` works as before
- [ ] `synapse plexus.cone.chat` works (dot-notation not yet implemented)
- [x] `synapse plexus cone chat` still works
- [x] `--working_dir .` expands to absolute path
- [x] `$USER` in params expands
- [ ] Missing required path params get smart defaults (not yet integrated)
- [x] Boolean flags without values work
- [x] `--model haiku` (KindStringEnum) works
