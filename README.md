# Synapse

**A CLI that writes itself from your API.**

Synapse is the command-line frontend for [Plexus RPC](../plexus-protocol/) — a streaming JSON-RPC 2.0 protocol built for LLM orchestration. Define methods in Rust, and Synapse discovers them at runtime: commands, help text, parameter validation, and output rendering all come from the schema. No codegen, no static config.

```
                  ┌─────────────┐
                  │  Substrate   │  Rust backend hub
                  │  (Plexus)    │  hosts activations
                  └──────┬──────┘
                         │ WebSocket JSON-RPC 2.0
                         │ streaming responses
                  ┌──────┴──────┐
                  │   Synapse    │  Haskell CLI
                  │  discovers   │  builds commands from schema
                  │  & invokes   │  renders streaming output
                  └─────────────┘
```

## The Full Picture: Define → Call

### 1. Define a method in Rust

An **activation** is a namespace of related methods. You define one by annotating an impl block:

```rust
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// Event type — what the method streams back
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum EchoEvent {
    Echo { message: String, count: u32 },
}

// The activation
pub struct Echo;

#[plexus_macros::hub_methods(
    namespace = "echo",
    version = "1.0.0",
    description = "Echo messages back"
)]
impl Echo {
    #[plexus_macros::hub_method(
        description = "Echo a message back the specified number of times",
        params(
            message = "The message to echo",
            count = "Number of times to repeat (default: 1)"
        )
    )]
    async fn echo(
        &self,
        message: String,
        count: u32,
    ) -> impl Stream<Item = EchoEvent> + Send + 'static {
        stream! {
            for i in 0..count {
                yield EchoEvent::Echo {
                    message: message.clone(),
                    count: i + 1,
                };
            }
        }
    }

    #[plexus_macros::hub_method(
        description = "Echo a message once",
        params(message = "The message to echo")
    )]
    async fn once(
        &self,
        message: String,
    ) -> impl Stream<Item = EchoEvent> + Send + 'static {
        stream! {
            yield EchoEvent::Echo { message, count: 1 };
        }
    }
}
```

The macro generates the JSON-RPC server, `Activation` trait impl, method enum, and JSON Schemas — all from the function signatures and `JsonSchema` derives. Register it with the hub:

```rust
let hub = DynamicHub::new("substrate")
    .register(Echo::new())
    .register(Health::new())
    .register(Cone::new());
```

That's the entire backend. No route tables, no handler boilerplate, no schema files.

### 2. Call it from Synapse

```bash
# Install
cd synapse && cabal install

# Synapse discovers the echo activation and its methods automatically
$ synapse substrate echo once --message "hello"
message: hello
count: 1

$ synapse substrate echo echo --message "hello" --count 3
message: hello
count: 1

message: hello
count: 2

message: hello
count: 3
```

Every method you add to the Rust backend is immediately available as a CLI command. The parameter names, types, descriptions, and validation all come from the schema.

### 3. See what's on the wire

```bash
# Dry-run: show the JSON-RPC request without sending
$ synapse --dry-run substrate echo echo --message "hello" --count 3
{"method":"echo.echo","params":{"message":"hello","count":3}}

# JSON mode: see the raw stream items
$ synapse --json substrate echo echo --message "hello" --count 3
{"type":"data","content":{"event":"echo","message":"hello","count":1}}
{"type":"data","content":{"event":"echo","message":"hello","count":2}}
{"type":"data","content":{"event":"echo","message":"hello","count":3}}
{"type":"complete"}

# Fetch the JSON Schema for the activation
$ synapse --schema substrate echo
{"namespace":"echo","methods":[...]}
```

## Progressive Discovery

Synapse navigates the backend like a filesystem. Each level shows what's available:

```bash
# No args — list backends
$ synapse
Available backends:
  substrate      127.0.0.1:4444 [OK]

# Backend name — list activations
$ synapse substrate
  arbor               Tree-structured conversation storage
  bash                Execute bash commands
  cone                Conversational AI sessions
  echo                Echo messages back
  health              Health check endpoint

# Activation — list methods
$ synapse substrate echo
  echo                Echo a message back the specified number of times
  once                Echo a message once

# Method with required params but no args — show help
$ synapse substrate cone chat
chat - Conversational interaction with a cone

  --identifier.type <string> (required)
      Discriminator for cone lookup (by_id, by_name)

  --identifier.name <string> (required if type=by_name)
  --prompt <string> (required)
      User message to send
```

## Parameters

Synapse parses flags into typed JSON using the schema's IR:

```bash
# Simple flags
synapse substrate echo echo --message "hello" --count 3

# Dotted keys for nested objects
synapse substrate cone chat \
  --identifier.type by_name \
  --identifier.name my-assistant \
  --prompt "hello"

# Repeated flags for arrays
synapse substrate tags set --tags backend --tags critical

# Boolean flags (bare flag = true)
synapse substrate cone create --ephemeral

# Raw JSON override
synapse substrate cone chat -p '{
  "identifier": {"type": "by_name", "name": "my-assistant"},
  "prompt": "hello"
}'
```

Path parameters (`--path`, `--working_dir`, etc.) expand `~`, resolve relative paths, and substitute `$ENV` variables automatically.

## Output Rendering

Synapse renders streaming responses through a Mustache template pipeline:

```bash
# Default: schema-aware pretty output
synapse substrate health check
status: healthy
uptime: 12345

# --raw: content JSON only (skip template rendering)
synapse --raw substrate health check
{"status":"healthy","uptime":12345}

# --json: full JSON-RPC stream items
synapse --json substrate health check
{"type":"data","content":{"status":"healthy","uptime":12345}}
```

Templates are resolved in order: `.substrate/templates/{namespace}/{method}.mustache` (project-local) → `~/.config/synapse/templates/` (user) → YAML-like fallback.

Generate templates from the schema:

```bash
synapse --generate-templates substrate
```

## Backend Discovery

Synapse finds backends through a registry or direct connection:

```bash
# Specify host and port (locates the registry, not the target)
synapse -H 192.168.1.10 -P 4444 substrate echo once --message "hi"

# Scan the local port range for Plexus servers
synapse _self scan
Found 3 backend(s):
  4444  substrate
  4445  plexus-dev
  4446  secrets
```

Backends are pinged in parallel with a 300ms timeout. The registry resolves backend names to host:port pairs, so `synapse substrate` connects to wherever substrate is running.

## Code Generation

Synapse emits a structured Intermediate Representation for building typed clients:

```bash
$ synapse --emit-ir substrate > substrate.ir.json
```

```json
{
  "irVersion": "2.0",
  "irTypes": {
    "echo.EchoEvent": {
      "tdKind": {
        "tag": "KindEnum",
        "keDiscriminator": "event",
        "keVariants": [
          {"vdName": "echo", "vdFields": [
            {"fdName": "message", "fdType": {"tag": "RefPrimitive", "contents": ["string", null]}},
            {"fdName": "count", "fdType": {"tag": "RefPrimitive", "contents": ["integer", "uint32"]}}
          ]}
        ]
      }
    }
  },
  "irMethods": {
    "echo.echo": {
      "mdStreaming": true,
      "mdParams": [
        {"pdName": "message", "pdType": {"tag": "RefPrimitive", "contents": ["string", null]}, "pdRequired": true},
        {"pdName": "count", "pdType": {"tag": "RefPrimitive", "contents": ["integer", "uint32"]}, "pdRequired": true}
      ],
      "mdReturns": {"tag": "RefNamed", "contents": ["echo", "EchoEvent"]}
    }
  }
}
```

Types are deduplicated by content hash across namespaces. Streaming is inferred from return type structure (enum with >1 non-error variant = streaming).

## Error Messages

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

$ synapse substrate cone chat --mesage "hello"
Unknown parameter: --mesage

Did you mean: --message?
```

## Architecture

Synapse treats the plugin hierarchy as a **category** and implements operations as **algebras** over recursion schemes:

```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

-- Navigate to a path (paramorphism — fold with access to original structure)
navigate :: Path -> SynapseM SchemaView

-- Walk the full tree (hylomorphism — fused unfold + fold, no intermediate tree)
walkSchema :: (SchemaF a -> SynapseM a) -> Path -> SynapseM a

-- Build IR (hylomorphism with parallel child fetching)
buildIR :: Path -> SynapseM IR
```

The base functor:

```haskell
data SchemaF a
  = PluginF PluginSchema Path [a]   -- Interior node (namespace with children)
  | MethodF MethodSchema Text Path  -- Leaf (invocable method)
```

Schemas are fetched lazily during navigation, cached by content hash, with cycle detection for recursive plugin graphs.

## Project Structure

```
synapse/
  app/Main.hs              # CLI entry, two-phase arg parsing, dispatch
  src/Synapse/
    Monad.hs               # SynapseM effect stack, error types
    Transport.hs           # WebSocket JSON-RPC bridge
    Schema/
      Types.hs             # Path, PluginSchema, MethodSchema, NavError
      Functor.hs           # SchemaF base functor, Fix
    Algebra/
      Recursion.hs         # cata, ana, hylo, para, apo (pure + monadic)
      Navigate.hs          # Path navigation (paramorphism)
      Walk.hs              # Tree walking (hylomorphism)
      Render.hs            # Schema → text rendering
    CLI/
      Parse.hs             # IR-driven --flag parsing → typed JSON
      Help.hs              # Auto-generated help from IR
      Transform.hs         # Path expansion, env var substitution
      Template.hs          # Mustache template generation from IR
    IR/
      Types.hs             # IR, TypeDef, MethodDef, TypeRef
      Builder.hs           # Schema → IR (hylomorphism + type dedup)
    Backend/
      Discovery.hs         # Registry discovery, health checks
    Self/
      Commands.hs          # _self meta-commands (scan, templates)

../plexus-protocol/        # Shared Plexus types (sibling package)
```

## Development

```bash
cabal build                        # Build
cabal test                         # Run tests (needs backend on localhost:4444)
cabal install                      # Install synapse binary
cabal build -f build-examples      # Build optional examples
```

## License

MIT
