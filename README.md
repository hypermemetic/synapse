```
███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗
██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝ \│/
███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  ──◉──
╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝   /│\
███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗
╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝
```

# Synapse

**A CLI that writes itself from your API's structure.**

Synapse is a schema-driven command-line interface that discovers and navigates API structures at runtime. Point it at a hub, and it generates commands, help text, and completions from the schema - no code generation step required.

---

## Quick Start

### Installation

```bash
cd synapse
cabal build
cabal install
```

### Basic Usage

Running `synapse` without arguments shows available backends:

```bash
$ synapse
Available backends:
  plexus - Algebraic CLI for Plexus hub
```

Connect to a backend by name:

```bash
$ synapse plexus
plexus v0.2.6

Modular plugin hub with dynamic activation

activations

  arbor        Conversation tree storage
  changelog    Track plexus schema changes
  claudecode   Claude Code session manager
  cone         LLM agent manager
  echo         Echo messages back
  health       Check hub health
  mustache     Template rendering service
  solar        Solar system model

methods

  call         Route a call to a registered activation
  hash         Get plexus configuration hash
```

### Navigating

Navigate into plugins like a filesystem:

```bash
$ synapse plexus echo
echo v0.1.0

Echo messages back

methods

  echo         Echo a message back
               --message <string>   The message to echo
               --count <integer>?   Number of times to repeat

  once         Echo a simple message once
               --message <string>   The message to echo
```

### Invoking Methods

Methods with required parameters show help:

```bash
$ synapse plexus echo once
once - Echo a simple message once

  --message <string> (required)
      The message to echo
```

Pass parameters inline with `--key value`:

```bash
$ synapse plexus echo once --message "Hello, world!"
Hello, world!
```

Or use JSON with `-p`:

```bash
$ synapse plexus echo echo -p '{"message": "hi", "count": 3}'
hi hi hi
```

Methods without required parameters auto-invoke:

```bash
$ synapse plexus health check
status: healthy
uptime_seconds: 12345
```

---

## Core Concepts

### Schema-Driven

Synapse fetches the hub's schema at runtime and derives the entire CLI from it:

- **Commands** come from plugin namespaces and method names
- **Help text** comes from descriptions in the schema
- **Parameters** come from JSON Schema definitions
- **Validation** happens based on `required` fields

There's no static code generation - the CLI adapts to whatever the hub exposes.

### Navigation

Navigation works like a filesystem path:

```
synapse plexus solar earth luna info
        |      |     |     |    +-- method
        |      |     |     +-- plugin (moon)
        |      |     +-- plugin (planet)
        |      +-- plugin (solar system)
        +-- backend
```

At each level, you can stop to see what's available. The schema is fetched lazily as you descend.

### Methods vs Plugins

- **Plugins** are namespaces containing methods and child plugins
- **Methods** are actions you can invoke

Plugins can nest arbitrarily deep. Methods are always leaves - you can't navigate into them.

### Parameters

Parameters can be passed two ways:

**Inline flags** (converted to JSON automatically):

```bash
synapse plexus echo echo --message "hello" --count 3
```

**JSON object** with `-p`:

```bash
synapse plexus echo echo -p '{"message": "hello", "count": 3}'
```

Inline flags support type inference: `true`/`false` become booleans, numbers become numbers, everything else is a string.

---

## Output Modes

### Default (Template Mode)

Output is formatted using Mustache templates when available, falling back to pretty-printed YAML-like output:

```bash
$ synapse plexus solar observe
name: Sol
planet_count: 8
asteroid_belt: true
```

### JSON Mode (`--json`)

Output raw JSON stream items (useful for piping):

```bash
$ synapse plexus solar observe --json
{"type":"data","provenance":["solar"],"hash":"abc123","content_type":"solar.observe","data":{"name":"Sol",...}}
```

### Raw Mode (`--raw`)

Output just the content JSON (skip template rendering):

```bash
$ synapse plexus solar observe --raw
{"name":"Sol","planet_count":8,"asteroid_belt":true}
```

### Schema Mode (`--schema`)

Fetch the raw JSON Schema for a path:

```bash
$ synapse plexus echo --schema
{"namespace":"echo","version":"0.1.0","methods":[...],"children":null}
```

### Dry Run (`--dry-run`)

Preview the JSON-RPC request without sending:

```bash
$ synapse plexus echo once --message "test" --dry-run
{"jsonrpc":"2.0","id":1,"method":"plexus.call","params":{"method":"plexus.echo.once","params":{"message":"test"}}}
```

---

## Architecture Overview

Synapse treats the plugin system as a **category**:

- **Objects** are schemas (plugins and methods)
- **Morphisms** are paths (sequences of namespace segments)

The CLI implements **algebras** over this category. Each operation (navigation, rendering, completion) is a specific algebra applied via recursion schemes.

### The SynapseM Monad

The effect stack:

```haskell
SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)
```

This provides:

- **Error handling** via `SynapseError` (navigation errors, transport errors, parse errors)
- **Environment** with schema cache and cycle detection
- **IO** for network calls

The environment tracks:

```haskell
data SynapseEnv = SynapseEnv
  { seHost    :: Text                              -- Hub host
  , sePort    :: Int                               -- Hub port
  , seCache   :: IORef (HashMap PluginHash PluginSchema)  -- Content-addressed cache
  , seVisited :: IORef (HashSet PluginHash)        -- Cycle detection
  }
```

---

## The Algebraic Approach

### Why Category Theory?

The plugin graph has recursive structure - plugins contain plugins contain plugins. Category theory provides tools for working with such structures:

1. **Extensibility**: New operations are just new algebras
2. **Correctness**: Recursion schemes ensure we handle all cases
3. **Fusion**: Hylomorphisms avoid building intermediate structures

### Base Functor

The schema tree is defined by its "shape":

```haskell
data SchemaF a
  = PluginF PluginSchema Path [a]   -- Interior node with children
  | MethodF MethodSchema Text Path  -- Leaf node
```

The type parameter `a` represents "what's in the recursive positions."

### Catamorphism (Fold)

A catamorphism collapses a structure bottom-up:

```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

For collecting methods:

```haskell
methodAlgebra :: SchemaF [MethodInfo] -> [MethodInfo]
methodAlgebra (PluginF schema path children) =
  localMethods ++ concat children
methodAlgebra (MethodF method ns path) =
  [MethodInfo method path ns]
```

### Paramorphism (Fold with Context)

Navigation needs to inspect the original structure while folding:

```haskell
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
```

At each layer, we receive `(original subtree, already-computed result)` pairs. This lets us check namespaces before recursing.

### Hylomorphism (Unfold + Fold, Fused)

Most operations combine unfolding (fetching schemas) with folding (processing):

```haskell
hyloM :: (Monad m, Traversable f)
      => (f b -> m b)      -- algebra (fold one step)
      -> (a -> m (f a))    -- coalgebra (unfold one step)
      -> a -> m b

walkMethods :: Path -> SynapseM [MethodInfo]
walkMethods = hyloM methodAlgebraM schemaCoalgebra
```

The fused version never materializes the intermediate tree.

---

## Template System

### Mustache Templates

Output is formatted using Mustache templates. Templates are resolved by content type (e.g., `echo.once`).

### Resolution Order

1. **Project-local**: `.substrate/templates/{namespace}/{method}.mustache`
2. **User global**: `~/.config/synapse/templates/{namespace}/{method}.mustache`
3. **Namespace default**: `{path}/{namespace}/default.mustache`
4. **Global default**: `{path}/default.mustache`

### Generating Templates

Scaffold templates from the schema:

```bash
$ synapse plexus --generate-templates
Generating templates in .substrate/templates...
  echo/once.mustache
  echo/echo.mustache
  solar/observe.mustache
  ...
Generated 15 templates
```

Generated templates use the method's return schema to create appropriate Mustache syntax:

```mustache
{{! echo.once }}
{{message}}
```

---

## Code Generation

### IR Output

Synapse can emit an Intermediate Representation for transpilers:

```bash
$ synapse plexus --emit-ir > plexus.ir.json
```

### IR Structure

```json
{
  "irVersion": "1.0",
  "irTypes": {
    "Position": {
      "tdName": "Position",
      "tdKind": { "tag": "KindStruct", "ksFields": [...] }
    }
  },
  "irMethods": {
    "cone.chat": {
      "mdName": "chat",
      "mdFullPath": "cone.chat",
      "mdStreaming": true,
      "mdParams": [...],
      "mdReturns": { "tag": "RefNamed", "contents": "ChatEvent" }
    }
  },
  "irPlugins": {
    "cone": ["create", "chat", "list", "delete"]
  }
}
```

Key features:

- **Deduplicated types**: Types from `$defs` are hoisted to top level
- **Streaming inference**: Methods returning discriminated unions are marked as streaming
- **Type references**: Methods reference types by name, not inline

This IR is consumed by codegen tools to produce TypeScript, Python, Swift, etc.

---

## Internals Deep Dive

### Schema Functor

The base functor captures one layer of tree structure:

```haskell
data SchemaF a
  = PluginF
      { sfSchema   :: PluginSchema  -- Schema at this node
      , sfPath     :: Path          -- Path to this node
      , sfChildren :: [a]           -- Recursive positions
      }
  | MethodF
      { sfMethod    :: MethodSchema
      , sfNamespace :: Text
      , sfMethodPath :: Path
      }
```

The fixed point gives the full tree:

```haskell
type SchemaTree = Fix SchemaF
```

### Cycle Detection

Plugins can reference each other via hashes. Synapse tracks visited hashes:

```haskell
checkCycle :: PluginHash -> Path -> SynapseM ()
checkCycle hash path = do
  visitedRef <- asks seVisited
  visited <- liftIO $ readIORef visitedRef
  when (hash `HS.member` visited) $
    throwNav $ Cycle hash path
  liftIO $ modifyIORef' visitedRef (HS.insert hash)
```

### Content-Addressed Caching

Schemas are cached by their content hash:

```haskell
fetchCached :: PluginHash -> SynapseM PluginSchema -> SynapseM PluginSchema
fetchCached hash fetcher = do
  cached <- lookupCache hash
  case cached of
    Just schema -> pure schema
    Nothing -> do
      schema <- fetcher
      insertCache (psHash schema) schema
      pure schema
```

Same hash = same content, so caching is always safe.

### Streaming Transport

Method invocation streams results:

```haskell
invokeStreaming :: Path -> Text -> Value
                -> (PlexusStreamItem -> IO ())
                -> SynapseM ()
```

Stream items include:

- `StreamData`: Result data with content type
- `StreamProgress`: Progress updates
- `StreamError`: Error messages
- `StreamDone`: Completion signal

---

## Development

### Building

```bash
cabal build
```

### Testing

```bash
cabal test
```

Tests are in `test/CLISpec.hs` and use a simple pattern:

```haskell
it "echo once" $ ["plexus", "echo", "once"] `has` ["once", "--message", "required"]
```

### Adding Backends

1. Add a `Backend` entry to `availableBackends` in `Main.hs`:

```haskell
availableBackends =
  [ Backend "plexus" "Algebraic CLI for Plexus hub" "127.0.0.1" 4444
  , Backend "mybackend" "Description" "host" 5555  -- new
  ]
```

2. Add a command variant and parser in the argument parsing section

3. The transport layer handles the connection - backends just need host/port

### Project Structure

```
synapse/
  app/
    Main.hs              -- CLI entry point, argument parsing
  src/Synapse/
    Monad.hs             -- SynapseM effect stack
    Transport.hs         -- Network layer
    Cache.hs             -- Content-addressed schema cache
    Renderer.hs          -- Template-based output
    Schema/
      Types.hs           -- Core types (Path, SchemaView, NavError)
      Functor.hs         -- SchemaF base functor
    Algebra/
      Recursion.hs       -- Pure recursion schemes (cata, ana, hylo, para, apo)
      Navigate.hs        -- Navigation (effectful paramorphism)
      Render.hs          -- Schema rendering to text
      Walk.hs            -- Tree walking (hylomorphism)
      Complete.hs        -- Shell completion
      TemplateGen.hs     -- Template generation from schemas
    IR/
      Types.hs           -- IR data types
      Builder.hs         -- Schema -> IR transformation
  test/
    CLISpec.hs           -- Integration tests
```

---

## Architecture Documentation

Detailed architecture docs, decision records, and implementation notes are in [`docs/architecture/`](./docs/architecture/).

See the [Architecture Index](./docs/architecture/INDEX.md) for a curated list of important documents.

Recent updates:
- **[Array and Normalization Fixes](./docs/architecture/16677498315934040575_array-and-normalization-fixes.md)** - CLI parser improvements for arrays and hyphenated method names
- **[Dynamic CLI Implementation](./docs/architecture/16680974430007725567_dynamic-cli-implementation.md)** - How the schema-driven CLI works
- **[Schema to CLI Pipeline](./docs/architecture/16680606527890548735_schema-to-cli-pipeline.md)** - Parameter parsing and type correspondence

---

## License

MIT
