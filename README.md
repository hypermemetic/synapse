# Synapse

```
███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗
██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝
███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗
╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝
███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗
╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝
```

**Algebraic CLI for Plexus**

Synapse navigates plugin hierarchies using coalgebraic lazy evaluation. Schemas are fetched on demand as you traverse the tree.

## The Algebra

```
Navigation : Path → SynapseM SchemaView
Rendering  : ShallowSchema → Text
Completion : ShallowSchema → [Text]
```

Schemas form a **free category**:
- **Objects**: Plugins identified by content hash
- **Morphisms**: Paths (composable sequences of child references)
- **Identity**: Empty path returns current schema
- **Composition**: Path concatenation chains lazy fetches

The CLI implements a **hylomorphism**:
```
Path → navigatePath (anamorphism) → Schema → render/invoke (catamorphism) → Output
```

## Usage

```bash
# Navigate to a plugin
synapse solar

# Navigate deeper
synapse solar earth luna

# Invoke a method
synapse health check
synapse solar observe

# Methods with required params show help
synapse echo once
# echo.once - Echo a simple message once
#   --message <string>  The message to echo

# Pass parameters inline
synapse echo once message=hello
synapse echo echo message=hi count=3

# Or as JSON
synapse echo once -p '{"message":"hello"}'
```

## Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--host` | `-H` | Plexus server host (default: 127.0.0.1) |
| `--port` | `-P` | Plexus server port (default: 4444) |
| `--json` | `-j` | Output raw JSON stream |
| `--dry-run` | `-n` | Show JSON-RPC request without sending |
| `--schema` | `-s` | Fetch raw schema JSON for path |
| `--params` | `-p` | Method parameters as JSON object |
| `--rpc` | `-r` | Raw JSON-RPC passthrough |

## Examples

```bash
# Root schema (lists all activations)
synapse

# Get raw schema JSON
synapse --schema solar

# Dry run (see the RPC without sending)
synapse -n echo once message=test

# Raw JSON-RPC passthrough
synapse --rpc '{"method":"health.check"}'

# Nested navigation
synapse solar earth luna info
```

## Architecture

```
synapse/
├── app/
│   └── Algebra.hs           # CLI entry point
└── src/Synapse/
    ├── Monad.hs             # SynapseM effect stack
    ├── Transport.hs         # WebSocket RPC client
    ├── Schema/
    │   ├── Types.hs         # PluginSchema, MethodSchema, ChildSummary
    │   └── Base.hs          # ShallowSchema (base functor)
    └── Algebra/
        ├── Navigate.hs      # Navigation algebra (anamorphism)
        └── Render.hs        # Rendering algebra (catamorphism)
```

### Effect Stack

```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

data SynapseEnv = SynapseEnv
  { seHost    :: Text
  , sePort    :: Int
  , seCache   :: IORef SchemaCache
  , seVisited :: IORef (Set PluginHash)  -- cycle detection
  }
```

### Shallow Schemas

Schemas on the wire are shallow — children are summaries, not full schemas:

```haskell
data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
```

Navigation fetches child schemas lazily via `{path}.schema` RPC calls.

## Build

```bash
cabal build synapse
cabal run synapse -- health check
```

## Test

```bash
cabal test synapse-test
```

The test suite uses a terse DSL:
```haskell
it "echo once" $ ["echo", "once"] `has` ["once", "--message", "required"]
it "invoke"    $ call ["echo", "once"] (msg "yo") `hasA` ["yo"]
```

## Documentation

- `docs/architecture/` — Design documents and implementation notes
- `docs/SYNAPSE.md` — Full categorical design specification

## License

MIT
