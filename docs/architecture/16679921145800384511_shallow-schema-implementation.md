# Shallow Schema Implementation

**Status: IMPLEMENTED**
**Date: 2024-12-27**

## Summary

Synapse CLI updated to work with shallow schemas. The schema is no longer recursive on the wire — children are summaries (namespace, description, hash), not full schemas. Navigation fetches child schemas lazily.

## What Changed

### Types (`meaning/src/Plexus/Schema/Recursive.hs`)

```haskell
-- Before: recursive
data PluginSchema = PluginSchema
  { ...
  , psChildren :: Maybe [PluginSchema]  -- full schemas
  }

-- After: shallow
data PluginSchema = PluginSchema
  { ...
  , psChildren :: Maybe [ChildSummary]  -- references only
  }

data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
```

### CLI (`synapse/app/Algebra.hs`)

- Changed from nested optparse-applicative subcommands to positional path arguments
- Schema fetched lazily via `plexus_call` with `{path}.schema`
- Multiple parameter input modes (see below)

```bash
synapse solar earth luna info              # navigates lazily, invokes method
synapse echo once message=hello            # key=value inline params
synapse echo once -p '{"message":"hello"}' # JSON params
synapse --schema solar                     # raw schema JSON
synapse --rpc '{"method":"health.check"}'  # raw JSON-RPC passthrough
```

### CLI Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--host` | `-H` | Plexus server host (default: 127.0.0.1) |
| `--port` | `-P` | Plexus server port (default: 4444) |
| `--json` | `-j` | Output raw JSON stream |
| `--dry-run` | `-n` | Show JSON-RPC request without sending |
| `--schema` | `-s` | Fetch raw schema JSON for path |
| `--params` | `-p` | Method parameters as JSON object |
| `--rpc` | `-r` | Raw JSON-RPC request (bypass navigation) |

### Parameter Input Modes

1. **Inline `key=value`**: `synapse echo echo message=hi count=3`
2. **JSON via `-p`**: `synapse echo once -p '{"message":"hello"}'`
3. **Auto-invoke**: Methods with no required params run automatically

## Debugging with Raw JSON-RPC

When synapse isn't available or for debugging, use `websocat` directly. The Plexus server runs on `ws://localhost:4444`.

### Get Root Schema

```bash
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"plexus.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Get Child Schema

```bash
# Solar system schema
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"solar.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Nested child schema
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"solar.earth.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Invoke Methods

```bash
# Health check
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"health.check"}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Echo with params
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"echo.once","params":{"message":"hello"}}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Observe solar system
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_call","params":{"method":"solar.observe"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Interactive Session

```bash
websocat ws://127.0.0.1:4444
# Then type JSON-RPC requests manually
```

## Alignment with Category Theory

The design document describes a coalgebraic architecture:

| Concept | Design Doc | Implementation | Alignment |
|---------|------------|----------------|-----------|
| Objects | Schemas by hash | `PluginSchema` with `psHash` | ✓ |
| Morphisms | Paths (child refs) | `[Text]` path segments | ✓ |
| Identity | Empty path | `[]` returns current schema | ✓ |
| Composition | Path concat | `navigatePath` chains fetches | ✓ |
| Coalgebra | `schema : Plugin → F(ChildSummary)` | `fetchSchemaAt` | ✓ |
| Lazy unfold | Anamorphism | Fetch on navigation | ✓ |

**The categorical structure is preserved.** The implementation correctly models:
- Schemas as objects in a free category
- Paths as morphisms (composable sequences of child references)
- Lazy observation via coalgebraic unfold

## Shortcuts Taken

### 1. No Base Functor

**Design doc specifies:**
```haskell
data PluginSchemaF a = PluginSchemaF
  { psfNamespace :: Text
  , ...
  , psfChildren  :: Maybe [a]
  }
  deriving (Functor, Foldable, Traversable)

-- With recursion-schemes instances
type instance Base PluginSchema = PluginSchemaF
```

**Implementation:**
We removed `PluginSchemaF` entirely. With shallow schemas, `PluginSchema` is not recursive — children are `ChildSummary`, not `PluginSchema`. Recursion-schemes don't apply.

**Impact:** Cannot use `cata`, `para`, `ana` from recursion-schemes library. Not needed since we don't have recursive structure on the wire.

### 2. No Algebras as First-Class

**Design doc specifies:**
```haskell
type RenderAlg = PluginSchemaF Text -> Text
type NavAlg = PluginSchemaF (ChildSummary, NavResult) -> NavResult

renderSchema = cata renderAlg
navigate = para navAlg
```

**Implementation:**
Direct pattern matching on `PluginSchema`:
```haskell
renderSchema :: PluginSchema -> Text
renderSchema schema = T.unlines $ [psNamespace schema, ...]
                    <> map renderChildSummary (pluginChildren schema)
```

**Impact:** Algebras are implicit in the code, not reified as separate functions. Less composable but simpler. Could refactor later.

### 3. No Zipper/Comonad for Navigation

**Design doc specifies:**
```haskell
data SchemaZipper = SchemaZipper
  { focus   :: PluginSchema
  , context :: [PluginSchema]  -- breadcrumbs
  }
```

**Implementation:**
Path tracked as `[Text]` list. No zipper structure.

**Impact:** Cannot navigate "up" the tree without re-fetching. Acceptable since CLI is stateless — each invocation is a fresh traversal.

### 4. No Effect Stack

**Design doc specifies:**
```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

data SynapseEnv = SynapseEnv
  { seCache   :: IORef SchemaCache
  , seVisited :: IORef (Set PluginHash)  -- cycle detection
  }
```

**Implementation:**
Plain `IO` with manual error handling. No cache, no cycle detection.

**Impact:**
- Multiple fetches for the same schema (no caching)
- Cycles would cause infinite loop (cycle detection missing)
- Errors handled ad-hoc with `exitFailure`

### 5. No Parameter Validation

**Design doc specifies:**
```haskell
validate :: MethodSchema -> [(Text, Text)] -> Either ValidationError Value
validate method params = validateAgainstSchema (msParams method) ...
```

**Implementation:**
Parameters passed as raw JSON via `-p` flag. No validation against method's JSON Schema.

**Impact:** Invalid params sent to backend, which returns error. Could validate client-side.

### 6. No Shell Completion

**Design doc specifies:**
```haskell
completions :: PluginSchema -> [Text]
completions schema = map msName (psMethods schema)
                  <> map csNamespace (pluginChildren schema)
```

**Implementation:**
Not implemented. Would require generating completion scripts from schema.

### 7. ~~Simplified Parameter Parsing~~ (RESOLVED)

**Design doc specifies:**
```bash
synapse echo echo --message "hi" --count 3
```

**Implementation:**
```bash
synapse echo echo message=hi count=3
```

Used `key=value` syntax instead of `--key value` to avoid conflicts with optparse-applicative's flag parsing. Type inference converts `"true"`→Bool, `"3"`→Number, etc.

## What's Missing vs Design Doc

| Feature | Status | Priority |
|---------|--------|----------|
| Schema caching by hash | Missing | Medium |
| Cycle detection | Missing | Low (no cycles in current schema) |
| Client-side param validation | Missing | Low |
| Shell tab completion | Missing | Medium |
| `key=value` param syntax | ✓ Implemented | — |
| `--schema` flag | ✓ Implemented | — |
| Raw JSON-RPC passthrough | ✓ Implemented | — |
| REPL mode | Missing | Low |
| Streaming output rendering | Partial | Medium |

## The Hylomorphism

The design doc describes the full pipeline as a hylomorphism:

```
Address → fetch (ana) → Schema → fold (cata) → Result
```

Our implementation:
```
Path → navigatePath (ana over IO) → PluginSchema → render/invoke (direct) → Output
```

The anamorphism (unfold) is preserved — we lazily fetch schemas as we navigate. The catamorphism (fold) is implicit in rendering and parameter handling, but not reified as a proper algebra.

## Conclusion

The implementation captures the **essential categorical structure**:
- Free category of schemas and paths
- Coalgebraic lazy observation
- Composition of morphisms (path segments)

The **shortcuts** are primarily about:
- Not reifying algebras as first-class values
- No effect stack / caching / cycle detection
- Simplified parameter handling

These are pragmatic choices for a working CLI. The categorical foundation remains intact and can be elaborated as needed.

## Related Documents

- [Category Verification Report](16680127584921047551_category-verification-report.md) - Verification that schema forms free category
- [Universal Schema Exposure](../../../substrate/docs/architecture/16680127584921047551_universal-schema-exposure.md) - Backend changes enabling `.schema` on all plugins
- [Synapse Design Document](../SYNAPSE.md) - Full categorical design specification
