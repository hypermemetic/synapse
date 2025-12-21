# Synapse

```
███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗
██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝
███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗
╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝
███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗
╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝
```

**Haskell frontend for Plexus - Typed APIs for LLM orchestration**

Synapse is a dynamic CLI that discovers and exposes backend capabilities at runtime, providing type-safe command-line interfaces to LLM orchestration systems.

## Features

- **Dynamic Schema Discovery**: Automatically discovers available activations and methods from the Plexus backend
- **Typed CLI Flags**: Generates typed command-line flags from JSON Schema definitions
- **Template Rendering**: Customizable Mustache templates for clean output formatting
- **Smart Caching**: Efficient schema caching with hash-based invalidation
- **Live System Info**: Real-time backend health and activation status

## Installation

```bash
cabal install synapse
```

Or build from source:

```bash
git clone https://codeberg.org/hypermemetic/synapse
cd synapse
cabal build
cabal run synapse -- info
```

## Quick Start

**View system info:**
```bash
synapse info
```

**List available activations:**
```bash
synapse --help
```

**Interact with Claude Code:**
```bash
synapse claudecode chat --name my-session --prompt "hello"
```

**Manage conversation trees:**
```bash
synapse arbor tree-create --description "My conversation"
synapse arbor tree-list
```

**Chat with Cone:**
```bash
synapse cone chat --id my-cone --prompt "What is the meaning of life?"
```

**Execute bash commands:**
```bash
synapse bash execute --command "echo hello world"
```

## Architecture

Synapse consists of two packages:

- **`meaning`**: Core types and schemas (Plexus.Types, Plexus.Schema)
- **`synapse`**: Client library and CLI implementation

The CLI dynamically builds subcommands by:
1. Fetching the Plexus schema (`plexus_schema`)
2. Generating typed CLI parsers from JSON Schema
3. Mapping CLI args to RPC method calls

## Configuration

**Default endpoint:** `ws://127.0.0.1:4444`

Override with flags:
```bash
synapse --host 192.168.1.100 --port 5555 cone chat --prompt "hello"
```

**Template customization:**

Create custom output templates in `.substrate/templates/`:
```
.substrate/templates/
├── claudecode/
│   └── chat.mustache
└── cone/
    └── chat.mustache
```

**Schema caching:**

View cache status:
```bash
synapse cache status
```

Force refresh:
```bash
# Refresh is default behavior
synapse cone chat --prompt "hello"

# Use cached schema (faster):
synapse --no-refresh cone chat --prompt "hello"
```

## Available Activations

- **arbor**: Manage conversation trees with context tracking
- **bash**: Execute bash commands and stream output
- **claudecode**: Manage Claude Code sessions with Arbor-backed conversation history
- **cone**: LLM cone with persistent conversation context
- **health**: Check hub health and uptime

Each activation provides multiple methods. Use `synapse <activation> --help` to see available commands.

## Template Rendering

Synapse uses Mustache templates to format output. Example template for `claudecode chat`:

```mustache
{{{text}}}{{#tool_input}}

🔧 {{tool_name}} {{file_path}}{{#command}}"{{.}}"{{/command}}{{pattern}}
{{/tool_input}}
```

This renders:
- Chat content as plain text
- Tool uses with formatted parameters
- Clean streaming output

Disable rendering with `--no-render` for raw JSON output.

## Examples

**Start a chat session:**
```bash
synapse claudecode chat --name dev-session --prompt "create a fibonacci function"
```

**List conversation trees:**
```bash
synapse arbor tree-list --template pretty
```

**Get method schema:**
```bash
synapse --schema cone chat
```

**Direct RPC calls:**
```bash
synapse call plexus_schema '[]'
synapse call cone_chat '{"identifier":{"by_name":{"name":"test"}},"prompt":"hi"}'
```

## Development

**Build with examples:**
```bash
cabal build --flag=build-examples all
cabal run schema-discovery
```

**Run tests:**
```bash
cabal test all
```

**Check code:**
```bash
cabal check
```

## Documentation

- Architecture docs: `docs/architecture/`
- Cleanup plan: `docs/CLEANUP-PLAN.md`
- Haddock: `cabal haddock all`

## License

MIT

## Links

- Repository: https://codeberg.org/hypermemetic/synapse
- Issues: https://codeberg.org/hypermemetic/synapse/issues

---

Built with ❤️ for the Plexus ecosystem
