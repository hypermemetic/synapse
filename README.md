# Synapse

**Algebraic CLI for Plexus** — 1,700 lines of categorical machinery.

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
synapse echo once message=hello      # inline params
synapse echo once -p '{"message":"hello"}'  # JSON params
synapse --schema solar               # raw schema JSON
synapse --rpc '{"method":"health.check"}'   # raw JSON-RPC
```

## Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--host` | `-H` | Plexus host (default: 127.0.0.1) |
| `--port` | `-P` | Plexus port (default: 4444) |
| `--json` | `-j` | Raw JSON stream output |
| `--dry-run` | `-n` | Show request without sending |
| `--schema` | `-s` | Fetch raw schema JSON |
| `--params` | `-p` | JSON parameters |
| `--rpc` | `-r` | Raw JSON-RPC passthrough |

## Structure

```
synapse/
├── app/Main.hs                 # 291 lines - CLI
└── src/                        # 1,221 lines
    ├── Plexus.hs               # RPC exports
    ├── Plexus/Client.hs        # WebSocket client
    └── Synapse/
        ├── Monad.hs            # Effect stack
        ├── Transport.hs        # RPC transport
        ├── Cache.hs            # Schema cache
        ├── Schema/Types.hs     # PluginSchema, ChildSummary
        ├── Schema/Base.hs      # ShallowSchema functor
        └── Algebra/
            ├── Navigate.hs     # Anamorphism
            ├── Render.hs       # Catamorphism
            └── Complete.hs     # Completions
```

## Build & Test

```bash
cabal build exe:synapse
cabal test cli-test
```

## License

MIT
