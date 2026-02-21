# Synapse

**A CLI that generates itself from your RPC server's schema.**

Point synapse at any Plexus RPC server and it builds commands, help text, parameter validation, and completions at runtime—no code generation step required.

```bash
$ synapse substrate bash execute --command "echo hello"
line: hello
code: 0
```

The CLI is completely schema-driven. Add a method to your server, and synapse immediately exposes it as a command. Change a parameter, and the CLI validates it automatically.

## Quick Start

```bash
# Install
cd synapse && cabal install

# Run a method
synapse substrate health check

# Get help for any method
synapse substrate echo once

# Navigate like a filesystem
synapse substrate arbor tree_create --owner_id "user123"
```

## How It Works

Synapse fetches your RPC server's JSON Schema at runtime and builds the entire CLI from it:

- **Commands** come from method names (`bash.execute` → `synapse substrate bash execute`)
- **Parameters** come from JSON Schema definitions (`--command`, `--tree_id`)
- **Help text** comes from schema descriptions
- **Validation** is automatic (required fields, types, enums)

No static configuration. No code generation. Pure runtime discovery.

## Core Features

### Schema-Driven Everything

```bash
# Dry-run shows exactly what gets sent
$ synapse --dry-run substrate echo echo --message "hello" --count 3
{"method":"echo.echo","params":{"message":"hello","count":3}}

# Help is auto-generated from schema
$ synapse substrate cone chat
chat - Conversational interaction with a cone

  --identifier.type <string> (required)
      Discriminator for cone lookup (by_id, by_name)

  --identifier.id <uuid> (required if type=by_id)
  --identifier.name <string> (required if type=by_name)

  --prompt <string> (required)
      User message to send
```

### Backend Discovery

```bash
# List available backends
$ synapse
Available backends:
  substrate      127.0.0.1:4444 [OK]

# Scan network for RPC servers
$ synapse _self scan
Found 3 backend(s):
  4444  substrate
  4445  plexus-dev
  4446  secrets
```

### Flexible Parameters

```bash
# Dotted keys for nested objects
synapse substrate cone chat \
  --identifier.type by_name \
  --identifier.name my-assistant \
  --prompt "hello"

# Raw JSON with -p
synapse substrate cone chat -p '{
  "identifier": {"type": "by_name", "name": "my-assistant"},
  "prompt": "hello"
}'

# Both produce identical requests
```

### Output Modes

```bash
# Default: pretty-printed with Mustache templates
synapse substrate health check
status: healthy
uptime: 12345

# JSON: raw stream items
synapse --json substrate health check
{"type":"data","content":{"status":"healthy","uptime":12345}}

# Raw: just the content
synapse --raw substrate health check
{"status":"healthy","uptime":12345}

# Schema: fetch the JSON Schema
synapse --schema substrate echo
{"namespace":"echo","methods":[...]}
```

## Code Generation

Synapse emits an Intermediate Representation (IR) for transpilers to consume:

```bash
$ synapse --emit-ir substrate > substrate.ir.json
```

The IR is a deduplicated, structured representation of the entire schema:

```json
{
  "irVersion": "2.0",
  "irTypes": {
    "cone.ChatEvent": {
      "tdKind": {
        "tag": "KindEnum",
        "keDiscriminator": "type",
        "keVariants": [
          {"vdName": "token", "vdFields": [...]},
          {"vdName": "done", "vdFields": [...]}
        ]
      }
    }
  },
  "irMethods": {
    "cone.chat": {
      "mdStreaming": true,
      "mdParams": [...],
      "mdReturns": {"tag": "RefNamed", "contents": "cone.ChatEvent"}
    }
  }
}
```

This IR powers code generators that produce TypeScript, Python, Swift, and Rust clients.

## Architecture

Synapse treats the plugin system as a **category**:

- **Objects** are schemas (plugins and methods)
- **Morphisms** are paths (namespace sequences)

Operations are implemented as **algebras** over this category using recursion schemes:

```haskell
-- Navigate to a path using a paramorphism (fold with context)
navigate :: Path -> SynapseM SchemaView

-- Collect all methods using a catamorphism (bottom-up fold)
collectMethods :: SynapseM [MethodInfo]

-- Build IR using a hylomorphism (fused unfold + fold)
buildIR :: SynapseM IR
```

This gives us:

- **Extensibility**: New operations are just new algebras
- **Correctness**: Recursion schemes ensure we handle all cases
- **Performance**: Hylomorphisms avoid building intermediate trees

### The Effect Stack

```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)
```

Provides:
- **Error handling** (navigation errors, transport errors, parse errors)
- **Environment** with content-addressed schema cache
- **Cycle detection** for recursive plugin graphs
- **IO** for network calls

## Performance

Synapse is optimized for fast iteration:

- **Connection pooling**: WebSocket connections are reused across calls
- **Schema caching**: Content-addressed cache avoids redundant fetches
- **IR caching**: Generated IR is cached by backend hash

Typical performance (with warm cache):
```bash
$ time synapse --emit-ir substrate > /dev/null
real    0m0.094s  # <100ms including startup
```

## Error Messages

Synapse provides contextual error messages that guide you:

```bash
$ synapse substrate invalid-plugin
Command not found: 'invalid-plugin' at substrate

Available at substrate:
  Methods:
    schema              - Get schema information
    health              - Health check endpoint

  Child plugins:
    arbor               - Tree-structured conversation storage
    bash                - Execute bash commands
    cone                - Conversational AI sessions
    ...

$ synapse substrate cone chat --mesage "hello"
Unknown parameter: --mesage

Did you mean: --message?
```

## Development

```bash
# Build
cabal build

# Test (requires running backend at localhost:4444)
cabal test

# Install
cabal install
```

### Project Structure

```
synapse/
  app/Main.hs              # CLI entry point
  src/Synapse/
    Monad.hs               # Effect stack (SynapseM)
    Transport.hs           # WebSocket JSON-RPC layer
    Schema/Types.hs        # Core types (Path, PluginSchema, MethodSchema)
    Algebra/
      Navigate.hs          # Schema navigation (paramorphism)
      Walk.hs              # Tree walking (hylomorphism)
      Render.hs            # Pretty-printing schemas
    CLI/
      Parse.hs             # Flag parsing (--key value → JSON)
      Help.hs              # Auto-generated help text
      Template.hs          # Mustache template generation
    IR/
      Types.hs             # IR data types
      Builder.hs           # Schema → IR transformation
    Backend/Discovery.hs   # Backend discovery via registry
```

## License

MIT
