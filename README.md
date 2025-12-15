# Cognition Client (Haskell)

Haskell client library for the Cognition Pipeline hub. Provides typed APIs for all hub plugins over WebSocket JSON-RPC.

## Quick Start

```haskell
import Cognition.Rpc (connect, disconnect, defaultConfig)
import qualified Cognition.Loom as Loom
import qualified Cognition.Bash as Bash
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  conn <- connect defaultConfig

  -- Create a conversation tree
  S.mapM_ print $ Loom.treeCreate conn Nothing "my-agent"

  -- Execute a bash command
  S.mapM_ print $ Bash.execute conn "echo hello"

  disconnect conn
```

## Building

```bash
cabal build
```

## CLI Usage

The `cognition-cli` executable provides command-line access to all hub plugins:

```bash
# Build the CLI
cabal build cognition-cli

# Check hub health
cabal run cognition-cli -- health

# Execute bash commands
cabal run cognition-cli -- bash execute echo hello world

# Loom tree operations
cabal run cognition-cli -- loom tree list
cabal run cognition-cli -- loom tree create alice
cabal run cognition-cli -- loom tree render <tree_id>
cabal run cognition-cli -- loom tree get-skeleton <tree_id>

# Loom node operations
cabal run cognition-cli -- loom node create-text <tree_id> Hello world
cabal run cognition-cli -- loom node create-text-child <tree_id> <parent_id> Response text
cabal run cognition-cli -- loom node children <tree_id> <node_id>

# Loom context operations
cabal run cognition-cli -- loom context leaves <tree_id>
cabal run cognition-cli -- loom context path <tree_id> <node_id>
```

## Architecture

```
┌──────────────────────────────────────────────┐
│              Your Application                │
├──────────────────────────────────────────────┤
│  Cognition.Loom   Cognition.Bash   Cognition.Health  │  Typed Plugin APIs
├──────────────────────────────────────────────┤
│              Cognition.Rpc                   │  Core RPC (hubRpc)
├──────────────────────────────────────────────┤
│           WebSocket JSON-RPC                 │
└──────────────────────────────────────────────┘
                     │
                     ▼
              Hub (Rust server)
```

### Core Primitive

All plugin APIs are built on a single streaming RPC primitive:

```haskell
hubRpc :: HubConnection -> Text -> Value -> Stream (Of HubStreamItem) IO ()
```

This sends a JSON-RPC subscription request and yields `HubStreamItem` values as they arrive from the hub.

## Modules

### Cognition.Rpc

Low-level WebSocket JSON-RPC client:

- `connect` / `disconnect` - Connection management
- `hubRpc` - Core streaming RPC call
- `HubStreamItem` - Stream events (Progress, Data, Error, Done)

### Cognition.Loom

Typed API for conversation tree management:

**Tree Operations:**
- `treeCreate` - Create a new tree
- `treeGet` - Get complete tree with all nodes
- `treeGetSkeleton` - Get lightweight structure (no node data)
- `treeList` / `treeListScheduled` / `treeListArchived`
- `treeRender` - ASCII visualization of tree
- `treeClaim` / `treeRelease` - Reference counting
- `treeUpdateMetadata`

**Node Operations:**
- `nodeCreateText` - Create text node
- `nodeCreateExternal` - Create node with external handle
- `nodeGet` / `nodeGetChildren` / `nodeGetParent` / `nodeGetPath`

**Context Operations:**
- `contextListLeaves` - All leaf nodes
- `contextGetPath` - Full path data from root to node
- `contextGetHandles` - External handles in path

**Types:**
- `Tree`, `Node`, `NodeType` (Text | External)
- `Handle` - External data reference with source, version, identifier, metadata
- `TreeSkeleton`, `NodeSkeleton` - Lightweight representations
- `LoomEvent` - All event types for streaming responses

### Cognition.Bash

Execute bash commands with streaming output:

```haskell
execute :: HubConnection -> Text -> Stream (Of BashEvent) IO ()

data BashEvent
  = Stdout Text
  | Stderr Text
  | Exit Int
```

### Cognition.Health

Check hub health:

```haskell
check :: HubConnection -> Stream (Of HealthEvent) IO ()

data HealthEvent = Status
  { status        :: Text
  , uptimeSeconds :: Int
  , timestamp     :: Int
  }
```

## Testing

Run the integration test suite (requires hub to be running):

```bash
cabal test loom-test
```

Or directly:

```bash
cabal run cognition-cli -- health  # Verify hub is up
runhaskell test/LoomTest.hs
```

## RPC Method Naming

The hub uses underscore-separated method names:

| Haskell Function | RPC Method |
|-----------------|------------|
| `treeCreate` | `loom_tree_create` |
| `nodeCreateText` | `loom_node_create_text` |
| `Bash.execute` | `bash_execute` |
| `Health.check` | `health_check` |

## Example: Building a Conversation

```haskell
import Cognition.Rpc (connect, disconnect, defaultConfig)
import qualified Cognition.Loom as Loom
import qualified Streaming.Prelude as S

main :: IO ()
main = do
  conn <- connect defaultConfig

  -- Create tree
  Just (Loom.TreeCreated treeId) <- S.head_ $ Loom.treeCreate conn Nothing "agent"

  -- Get root node
  Just (Loom.TreeSkeletonData skel) <- S.head_ $ Loom.treeGetSkeleton conn treeId
  let rootId = Loom.treeSkeletonRoot skel

  -- Add messages
  Just (Loom.NodeCreated _ n1 _) <- S.head_ $
    Loom.nodeCreateText conn treeId (Just rootId) "User: Hello!" Nothing

  Just (Loom.NodeCreated _ n2 _) <- S.head_ $
    Loom.nodeCreateText conn treeId (Just n1) "Assistant: Hi there!" Nothing

  -- Render tree
  S.mapM_ (\(Loom.TreeRenderResult _ r) -> putStrLn r) $ Loom.treeRender conn treeId
  -- Output:
  -- └──
  --     └── User: Hello!
  --         └── Assistant: Hi there!

  disconnect conn
```

## File Structure

```
frontend-hs/
├── app/
│   └── Main.hs              -- CLI entry point
├── src/
│   └── Cognition/
│       ├── Rpc.hs           -- Re-exports
│       ├── Rpc/
│       │   ├── Types.hs     -- JSON-RPC types
│       │   └── Client.hs    -- WebSocket client
│       ├── Bash.hs          -- Bash plugin API
│       ├── Health.hs        -- Health plugin API
│       └── Loom.hs          -- Loom plugin API
├── test/
│   └── LoomTest.hs          -- Integration tests
└── cognition-client.cabal
```

## Dynamic CLI (`symbols-dyn`)

A schema-driven CLI that discovers commands at runtime from the substrate:

```bash
# List available commands
cabal run symbols-dyn

# Execute commands
cabal run symbols-dyn -- arbor tree-list
cabal run symbols-dyn -- cone list
```

### Output Modes

```bash
# Default: Template-rendered or pretty JSON
symbols-dyn arbor tree-list

# Raw JSON (for piping to jq)
symbols-dyn --raw arbor tree-list | jq '.tree_ids[0]'

# Full stream item as JSON
symbols-dyn --json arbor tree-list
```

### Schema Introspection

```bash
# Show schema for a namespace
symbols-dyn --schema arbor

# Show schema for a specific method
symbols-dyn --schema arbor tree-create
```

### Custom Templates

Templates use Mustache syntax and are loaded from:
1. `.substrate/templates/{namespace}/{method}.mustache` (project-local)
2. `~/.config/symbols/templates/{namespace}/{method}.mustache` (user-global)

**Template syntax:**
```mustache
{{variable}}              - Simple substitution
{{nested.path}}           - Nested path lookup
{{#array}}...{{/array}}   - Iterate over arrays
{{.}}                     - Current item in iteration
```

**Example template** (`.substrate/templates/cone/list.mustache`):
```mustache
Cones:
{{#cones}}  • {{name}} ({{id}})
    Model: {{model_id}}
    Tree:  {{head.tree_id}}
{{/cones}}
```

**Edit templates:**
```bash
# Open/create template for a method
symbols-dyn --template arbor tree-list
```

This opens `$EDITOR` (or vi) with the template file, creating a default template if it doesn't exist.
