# Synapse

**Algebraic CLI for Plexus** — categorical machinery for schema navigation.

## The Algebra

```
Navigation : Path → SynapseM SchemaView
Rendering  : ShallowSchema → Text
Completion : ShallowSchema → [Text]
```

Schemas form a **free category**. The CLI implements a **hylomorphism**:
```
Path → navigate (anamorphism) → Schema → render (catamorphism) → Output
```

## Usage

```bash
synapse                              # root schema
synapse solar                        # navigate to plugin
synapse solar earth luna             # nested navigation
synapse health check                 # invoke method (auto if no required params)
synapse echo once --message hello    # inline params
synapse echo once -p '{"message":"hello"}'  # JSON params
synapse --schema solar               # plugin schema JSON
synapse --schema echo once           # method schema JSON (compact)
synapse --rpc '{"method":"health.check"}'   # raw JSON-RPC
```

## Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--host` | `-H` | Plexus host (default: 127.0.0.1) |
| `--port` | `-P` | Plexus port (default: 4444) |
| `--json` | `-j` | Raw JSON stream output |
| `--dry-run` | `-n` | Show request without sending |
| `--schema` | `-s` | Fetch schema JSON (plugin or method) |
| `--params` | `-p` | JSON parameters |
| `--rpc` | `-r` | Raw JSON-RPC passthrough |

## Structure

```
synapse/
├── app/Main.hs                 # CLI entry point
└── src/Synapse/
    ├── Monad.hs                # Effect stack (SynapseM)
    ├── Transport.hs            # RPC transport layer
    ├── Cache.hs                # Schema cache
    ├── Schema/
    │   ├── Types.hs            # Re-exports from substrate-protocol
    │   └── Base.hs             # ShallowSchema functor
    └── Algebra/
        ├── Navigate.hs         # Anamorphism (path → schema)
        ├── Render.hs           # Catamorphism (schema → text)
        └── Complete.hs         # Completions

../substrate-protocol/          # Sibling package (shared types)
└── src/
    ├── Substrate/
    │   ├── Client.hs           # WebSocket connection
    │   └── Transport.hs        # Low-level RPC
    └── Plexus/
        ├── Types.hs            # PlexusStreamItem, Provenance
        └── Schema/Recursive.hs # PluginSchema, MethodSchema, SchemaResult
```

## Build & Test

```bash
cabal build exe:synapse
cabal test cli-test
```

## License

MIT
