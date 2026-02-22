# Synapse TypeScript REPL: Agent Programming Environment

## Overview

`synapse jsrepl` provides a full-featured TypeScript REPL where agents can write and execute code incrementally. Unlike traditional chat REPLs, this is a **programming environment** where:

- Variables and functions persist across interactions
- The generated TypeScript client (from synapse-cc) is pre-loaded and available
- Agents build programs line-by-line, similar to Python's ipython or Node's REPL
- Full TypeScript features: async/await, streams, imports, type inference
- The agent "lives inside" the program it's building

## Architecture

### Context Accumulation Strategy

jsexec spawns fresh V8 isolates per execution (ephemeral). To maintain state, the REPL accumulates all code and re-executes it on each interaction:

```
┌─────────────────────────────────────────────────────────────┐
│ Interaction 1                                               │
│ User: const x = 10                                          │
│ Execute: "import client from './client.ts';\nconst x = 10;" │
│ Output: undefined                                           │
│ State: [client setup, "const x = 10"]                       │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ Interaction 2                                               │
│ User: const y = x + 5                                       │
│ Execute: "import client from './client.ts';                 │
│           const x = 10;                                     │
│           const y = x + 5;"                                 │
│ Output: undefined                                           │
│ State: [client setup, "const x = 10", "const y = x + 5"]   │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ Interaction 3                                               │
│ User: console.log(y)                                        │
│ Execute: "import client from './client.ts';                 │
│           const x = 10;                                     │
│           const y = x + 5;                                  │
│           console.log(y);"                                  │
│ Output: 15                                                  │
│ State: [client setup, "const x = 10", ..., "console.log(y)"]│
└─────────────────────────────────────────────────────────────┘
```

### Integration with synapse-cc

At REPL startup:

1. Generate TypeScript client for backend: `synapse-cc generate typescript <backend> -o /tmp/client.ts`
2. Bundle client into REPL context as module
3. Auto-import: `import { createClient } from './client.ts'; const client = await createClient();`
4. Client is now available in all user code

### Command Structure

```bash
synapse jsrepl <backend> [OPTIONS]

Arguments:
  <backend>        Backend to connect to (substrate, plexus, etc.)

Options:
  --model <MODEL>              Model for agent-driven mode (sonnet, opus, haiku)
  --working-dir <DIR>          Working directory for file operations
  --save-session <FILE>        Save REPL session to file
  --load-session <FILE>        Load previous REPL session
  --no-client                  Don't auto-load generated client
  --prompt <TEXT>              Custom prompt (default: "ts> ")
  --multiline                  Start in multiline mode
  -h, --help                   Show help
```

## Use Cases

### 1. Manual Interactive Programming

User types TypeScript code line-by-line:

```bash
$ synapse jsrepl substrate

Client generated for backend 'substrate'
TypeScript REPL - type /help for commands

ts> const result = await client.bash.execute({command: 'ls -la'})
ts> for await (const item of result) {
...   if (item.type === 'line') console.log(item.line)
... }
total 48
drwxr-xr-x  8 user user 4096 Feb 22 10:00 .
drwxr-xr-x 12 user user 4096 Feb 22 09:30 ..
...

ts> const files: string[] = []
ts> for await (const item of client.bash.execute({command: 'ls'})) {
...   if (item.type === 'line') files.push(item.line)
... }
ts> files.filter(f => f.endsWith('.ts'))
['client.ts', 'main.ts', 'types.ts']

ts> /save work.ts
Session saved to work.ts
```

### 2. Agent-Driven Development

Agent uses REPL as its execution environment:

```bash
$ synapse jsrepl substrate --model sonnet

Agent: Claude Sonnet
TypeScript REPL - I'll help you build programs interactively

You: fetch the schema and count all methods

Agent: I'll use the client to fetch the schema and count methods.

> const schema = await client.schema({})
> const methods = []
> function collectMethods(obj: any) {
>   if (obj.methods) methods.push(...obj.methods)
>   if (obj.children) obj.children.forEach(collectMethods)
> }
> collectMethods(schema)
> console.log(`Found ${methods.length} methods`)

Found 47 methods

You: now group them by namespace

Agent: I'll group the methods by their namespace.

> const grouped = methods.reduce((acc, m) => {
>   const ns = m.namespace || 'root'
>   if (!acc[ns]) acc[ns] = []
>   acc[ns].push(m)
>   return acc
> }, {} as Record<string, any[]>)
> Object.entries(grouped).map(([ns, ms]) => `${ns}: ${ms.length}`)

['bash: 3', 'claudecode: 12', 'jsexec: 4', ...]
```

### 3. Script Building and Export

Build a reusable script incrementally:

```bash
ts> // Let's build a script to analyze plugin schemas
ts> interface PluginStats {
...   name: string
...   methodCount: number
...   hasChildren: boolean
... }

ts> async function analyzePlugin(path: string[]): Promise<PluginStats> {
...   const schema = await client.schema({path})
...   return {
...     name: path.join('.'),
...     methodCount: schema.methods?.length || 0,
...     hasChildren: !!schema.children
...   }
... }

ts> const stats = await analyzePlugin(['substrate', 'bash'])
ts> console.log(stats)
{ name: 'substrate.bash', methodCount: 3, hasChildren: false }

ts> /export analyze.ts
Exported session to analyze.ts

$ cat analyze.ts
import { createClient } from './client.ts';

const client = await createClient();

interface PluginStats {
  name: string;
  methodCount: number;
  hasChildren: boolean;
}

async function analyzePlugin(path: string[]): Promise<PluginStats> {
  const schema = await client.schema({path});
  return {
    name: path.join('.'),
    methodCount: schema.methods?.length || 0,
    hasChildren: !!schema.children
  };
}

// Usage example
const stats = await analyzePlugin(['substrate', 'bash']);
console.log(stats);
```

## Implementation Phases

### Phase 1: Basic REPL Loop with Context Accumulation

**Goal**: Working TypeScript REPL that maintains state across interactions

**Tasks**:
1. [ ] Create `Synapse.REPL.JsExec` module
   - File: `src/Synapse/REPL/JsExec.hs`
   - `ReplState` type for accumulating code
   - `runJsRepl :: ReplOpts -> SynapseM ()` main loop
   - Context accumulation logic

2. [ ] Implement context accumulation
   - Store each input line in state
   - Concatenate all lines before execution
   - Handle variable shadowing and redeclaration
   - Preserve `const`/`let` scoping

3. [ ] Basic input/output
   - Read line from stdin
   - Execute via `jsexec.execute`
   - Stream console output
   - Display result value
   - Show errors clearly

4. [ ] Integration with jsexec
   - Invoke `jsexec.execute` with accumulated code
   - Parse streaming output (`JsExecEvent`)
   - Handle console.log, console.error
   - Display final result

**Deliverable**: Basic working REPL

```bash
$ synapse jsrepl substrate
ts> const x = 10
undefined
ts> const y = 20
undefined
ts> x + y
30
ts> /quit
```

### Phase 2: Client Generation and Injection

**Goal**: Auto-generate and inject TypeScript client

**Tasks**:
1. [ ] Client generation at startup
   - Call `synapse-cc generate typescript <backend>`
   - Write to `/tmp/synapse-repl-<session-id>/client.ts`
   - Generate index file with re-exports

2. [ ] Module bundling
   - Use jsexec's module system
   - Bundle client.ts and dependencies
   - Make available as `./client.ts` import

3. [ ] Auto-import injection
   - Prepend import statement to context
   - Initialize client: `const client = await createClient()`
   - Make `client` available in all user code

4. [ ] Client configuration
   - Pass backend host/port to client
   - Configure timeouts and retry logic
   - Handle connection errors gracefully

**Deliverable**: REPL with pre-loaded client

```bash
$ synapse jsrepl substrate
Generating TypeScript client for 'substrate'...
Client ready at /tmp/synapse-repl-abc123/client.ts

ts> client
{ schema: [Function], bash: { execute: [Function] }, ... }

ts> await client.bash.execute({command: 'echo hello'})
AsyncIterator { ... }
```

### Phase 3: Advanced TypeScript Features

**Goal**: Full TypeScript REPL experience

**Tasks**:
1. [ ] Multiline input
   - Detect incomplete syntax (unclosed braces, etc.)
   - Show continuation prompt: `... `
   - Buffer lines until complete
   - `/multi` command for explicit multiline mode

2. [ ] Top-level await
   - Wrap code in async context
   - Handle await expressions
   - Stream async iteration results

3. [ ] Type inference display
   - Run TypeScript compiler in background
   - Show inferred types for variables
   - Display function signatures
   - Type errors as warnings (don't block execution)

4. [ ] Import support
   - Import from npm modules
   - Import local files
   - Dynamic imports
   - Bundle dependencies with esbuild

**Deliverable**: Full-featured TypeScript REPL

```bash
ts> for await (const item of client.bash.execute({command: 'ls'})) {
...   if (item.type === 'line') {
...     console.log(item.line)
...   }
... }
client.ts
main.ts
types.ts

ts> import { readFile } from 'fs/promises'
ts> const content = await readFile('package.json', 'utf-8')
ts> JSON.parse(content).name
'synapse'
```

### Phase 4: Meta-Commands and Session Management

**Goal**: REPL commands for productivity

**Tasks**:
1. [ ] Core commands
   - `/help` - Show help
   - `/quit`, `/q` - Exit
   - `/clear` - Clear screen
   - `/reset` - Reset REPL state

2. [ ] Session management
   - `/save <file>` - Save session to file
   - `/load <file>` - Load session from file
   - `/export <file>` - Export as standalone script
   - `/history` - Show command history

3. [ ] Code inspection
   - `/vars` - List all variables
   - `/funcs` - List all functions
   - `/type <expr>` - Show type of expression
   - `/context` - Show accumulated code

4. [ ] Execution control
   - `/time <expr>` - Time execution
   - `/trace` - Enable execution tracing
   - `/multi` - Toggle multiline mode

**Deliverable**: Productive REPL environment

```bash
ts> /vars
x: number = 10
y: number = 20
client: Client = { ... }
files: string[] = ['client.ts', 'main.ts']

ts> /type files
string[]

ts> /save my-work.session
Session saved to my-work.session (42 lines)

ts> /export script.ts
Exported executable script to script.ts
```

### Phase 5: Agent Integration

**Goal**: Enable agents to use REPL as execution environment

**Tasks**:
1. [ ] Agent mode
   - `--model` flag to enable agent
   - Agent receives REPL context
   - Agent can execute code on behalf of user
   - Show "thinking" before code execution

2. [ ] Structured output
   - Agent explains what code will do
   - Shows code before executing
   - Interprets results
   - Suggests next steps

3. [ ] Tool use integration
   - REPL is available as agent tool
   - Agent can propose code snippets
   - User approves before execution
   - Iterative refinement

4. [ ] Persistence and resumption
   - Save agent conversation + REPL state
   - Resume exactly where left off
   - Fork sessions to try alternatives

**Deliverable**: Agent-assisted programming environment

```bash
$ synapse jsrepl substrate --model sonnet

You: count all the TypeScript files in the current directory

Agent: I'll use the client to execute a bash command and filter the results.

Executing:
  const result = await client.bash.execute({command: 'find . -name "*.ts"'})
  const files = []
  for await (const item of result) {
    if (item.type === 'line') files.push(item.line)
  }
  files.length

Result: 12

There are 12 TypeScript files in the current directory.

You: now read each one and count total lines of code

Agent: I'll read each file and sum up the line counts.

Executing:
  let totalLines = 0
  for (const file of files) {
    const content = await client.bash.execute({
      command: `wc -l ${file}`
    })
    for await (const item of content) {
      if (item.type === 'line') {
        totalLines += parseInt(item.line.split(' ')[0])
      }
    }
  }
  totalLines

Result: 3847

Total lines of code across all TypeScript files: 3,847
```

## Data Structures

### Core Types

```haskell
-- REPL state
data ReplState = ReplState
  { rsLines       :: [Text]              -- Accumulated code lines
  , rsClientPath  :: Maybe FilePath      -- Path to generated client
  , rsBackend     :: Text                -- Backend name
  , rsSessionId   :: Text                -- Unique session ID
  , rsVariables   :: Map Text Value      -- Variable registry (for /vars)
  , rsMultiline   :: Bool                -- Multiline mode flag
  , rsHistory     :: [Text]              -- Command history
  }

-- REPL options
data JsReplOpts = JsReplOpts
  { jroBackend       :: Text
  , jroModel         :: Maybe Text       -- Enable agent mode
  , jroWorkingDir    :: Maybe FilePath
  , jroSaveSession   :: Maybe FilePath
  , jroLoadSession   :: Maybe FilePath
  , jroNoClient      :: Bool             -- Skip client generation
  , jroPrompt        :: Text             -- Custom prompt
  , jroMultiline     :: Bool             -- Start in multiline mode
  }

-- REPL commands
data ReplCommand
  = ReplQuit
  | ReplHelp
  | ReplClear
  | ReplReset
  | ReplSave FilePath
  | ReplLoad FilePath
  | ReplExport FilePath
  | ReplHistory
  | ReplVars
  | ReplFuncs
  | ReplType Text
  | ReplContext
  | ReplTime Text
  | ReplTrace
  | ReplMulti
  deriving stock (Show, Eq)

-- jsexec output event
data JsExecEvent
  = ConsoleLog Text
  | ConsoleError Text
  | Result Value
  | CompileError Text
  | RuntimeError Text
  deriving stock (Show, Eq)
```

### Client Generation

```haskell
-- Client info
data ClientInfo = ClientInfo
  { ciBackend    :: Text          -- Backend name
  , ciPath       :: FilePath      -- Path to client.ts
  , ciImports    :: Text          -- Import statements
  , ciSetup      :: Text          -- Initialization code
  }

-- Generate client for backend
generateClient :: Text -> IO ClientInfo
generateClient backend = do
  sessionId <- generateSessionId
  let dir = "/tmp/synapse-repl-" <> sessionId
  createDirectoryIfMissing True dir

  -- Call synapse-cc
  callProcess "synapse-cc"
    [ "generate", "typescript", T.unpack backend
    , "-o", dir </> "client.ts"
    ]

  let imports = "import { createClient } from './client.ts';"
      setup   = "const client = await createClient();"

  pure $ ClientInfo
    { ciBackend = backend
    , ciPath = dir </> "client.ts"
    , ciImports = imports
    , ciSetup = setup
    }
```

## File Structure

```
synapse/
├── app/
│   └── Main.hs                     # Add jsrepl command
├── src/
│   └── Synapse/
│       ├── REPL/
│       │   ├── JsExec.hs           # Main jsrepl module
│       │   ├── JsExec/
│       │   │   ├── Client.hs       # Client generation
│       │   │   ├── Commands.hs     # Meta-command parsing
│       │   │   ├── Context.hs      # Context accumulation
│       │   │   ├── Display.hs      # Output formatting
│       │   │   └── Agent.hs        # Agent integration
│       │   └── ...
│       └── ...
└── PLAN-JSREPL.md                  # This file
```

## Technical Challenges

### 1. Variable Redeclaration

**Problem**: TypeScript/JavaScript doesn't allow `const x = 1; const x = 2;`

**Solution**: Track declarations and transform to assignments:

```haskell
transformRedeclarations :: ReplState -> Text -> (ReplState, Text)
transformRedeclarations state input =
  case parseDeclaration input of
    Just (varName, kind, expr) ->
      if varName `elem` rsVariables state
      then (state, varName <> " = " <> expr)  -- Convert to assignment
      else (state { rsVariables = insert varName () }, input)
    Nothing -> (state, input)
```

### 2. Async Context Wrapping

**Problem**: Top-level await requires async context

**Solution**: Wrap all code in async IIFE:

```typescript
// User code:
const x = await fetch('/api')

// Executed as:
(async () => {
  const x = await fetch('/api')
})()
```

### 3. Result Display

**Problem**: How to show expression results without explicit `console.log`?

**Solution**: Wrap last expression in return:

```typescript
// User types: 2 + 2
// Execute:
(async () => {
  return 2 + 2
})()
// Display: 4

// User types: const x = 10
// Execute:
(async () => {
  const x = 10
  return x
})()
// Display: 10
```

### 4. Stream Handling

**Problem**: How to display streaming results interactively?

**Solution**: Collect stream items and display incrementally:

```typescript
// User code:
for await (const item of client.bash.execute({command: 'ls'})) {
  console.log(item)
}

// REPL displays each console.log as it arrives:
{ type: 'line', line: 'file1.ts' }
{ type: 'line', line: 'file2.ts' }
{ type: 'done', exit_code: 0 }
```

### 5. Error Recovery

**Problem**: Syntax errors should not crash REPL

**Solution**: Try-catch around execution, preserve state on error:

```haskell
executeInRepl :: ReplState -> Text -> IO (Either Text ReplState)
executeInRepl state input = do
  let newState = state { rsLines = rsLines state ++ [input] }
  result <- tryExecute (accumulate newState)
  case result of
    Left err -> do
      displayError err
      pure (Right state)  -- Don't save erroneous input
    Right _ ->
      pure (Right newState)  -- Save successful input
```

## Performance Considerations

### Execution Speed

- Each interaction re-executes **all** accumulated code
- For 100 lines, that's 100 lines re-parsed and re-executed
- jsexec startup is ~50-100ms (workerd process spawn)
- V8 JIT compilation helps with repeated execution

**Mitigation**:
- Keep REPL sessions focused (use /reset to clear)
- Snapshot state after certain operations
- Use /export to create standalone scripts

### Memory Usage

- Accumulated code stored as `[Text]` in memory
- Generated client bundled once
- jsexec process is ephemeral (no memory leak)

**Typical session**:
- 50 lines of code: ~5KB
- Generated client: ~100KB
- Total memory: < 1MB per session

### Network Latency

- Each client method call goes over network
- Streaming reduces perceived latency
- Can batch operations in single code block

## Testing Strategy

### Unit Tests

1. **Context accumulation**: Verify code properly concatenates
2. **Variable tracking**: Test redeclaration transformation
3. **Command parsing**: Verify all meta-commands parse
4. **Client generation**: Mock synapse-cc invocation

### Integration Tests

1. **Basic REPL flow**: Create session, execute code, verify state
2. **Client integration**: Generate client, execute methods, check results
3. **Multiline input**: Test function definitions, loops, etc.
4. **Error handling**: Syntax errors, runtime errors, network errors
5. **Session save/load**: Persist and restore state

### Manual Testing

1. **User workflow**: Interactive session with variable definitions, async operations
2. **Agent workflow**: Enable agent mode, give tasks, verify code execution
3. **Complex operations**: Streaming, async iteration, file operations
4. **Edge cases**: Very long sessions, large outputs, slow operations

## Success Metrics

1. **Usability**: Can define variables and use them in < 3 interactions
2. **Responsiveness**: Execution starts within 200ms
3. **Reliability**: Handles syntax errors without crashing
4. **Feature completeness**: Supports async/await, imports, top-level await
5. **Agent integration**: Agent can execute multi-step programs successfully

## Comparison with Other REPLs

| Feature | Node REPL | ts-node REPL | synapse jsrepl |
|---------|-----------|--------------|----------------|
| TypeScript | ❌ | ✅ | ✅ |
| Persistent state | ✅ | ✅ | ✅ (via re-execution) |
| Top-level await | ✅ | ✅ | ✅ |
| Pre-loaded client | ❌ | ❌ | ✅ |
| Agent integration | ❌ | ❌ | ✅ |
| Sandboxed execution | ❌ | ❌ | ✅ (V8 isolate) |
| Multi-backend | ❌ | ❌ | ✅ |

## Future Enhancements

1. **Incremental compilation**: Cache TypeScript compilation, only recompile changed lines
2. **Snapshot/restore**: Save execution state (not just source), restore instantly
3. **Time-travel debugging**: Step backwards through execution history
4. **Collaborative REPL**: Multiple users in same session
5. **Visual mode**: Rich terminal UI with panels for vars, output, errors
6. **Notebook mode**: Mix code cells with markdown documentation
7. **Plugin system**: Load custom REPL extensions
8. **Remote execution**: REPL on local machine, jsexec on remote server

## Dependencies

### New Dependencies

```yaml
# For readline support (if not using haskeline)
readline: "^1.0"

# For syntax highlighting (optional)
ansi-terminal: "^0.11"

# For TypeScript parsing (syntax validation)
# Could shell out to tsc or use tree-sitter
```

### External Tools

- `synapse-cc`: Must be installed and in PATH
- `jsexec` backend: Must be running and accessible
- `esbuild` (optional): For bundling user imports

## Migration Path

### From Node REPL

Users currently use:
```bash
node
> const client = require('./client.js')
> client.bash.execute({command: 'ls'})
```

New workflow:
```bash
synapse jsrepl substrate
ts> client.bash.execute({command: 'ls'})  # client pre-loaded
```

**Benefits**: Client generation automated, TypeScript support, agent integration

### From synapse CLI

Users currently use:
```bash
synapse substrate bash execute '{"command": "ls"}'
```

New workflow:
```bash
synapse jsrepl substrate
ts> await client.bash.execute({command: 'ls'})
```

**Benefits**: Reuse results, build on previous operations, full programming capability

## Example: Full Session

```bash
$ synapse jsrepl substrate --model sonnet

Generating TypeScript client for 'substrate'...
Client ready. TypeScript REPL v0.1.0
Agent: Claude Sonnet 4.5

Type /help for commands

You: find all TypeScript files and count their total lines

Agent: I'll search for .ts files and count the lines in each.

> const findResult = await client.bash.execute({
>   command: 'find . -name "*.ts" -type f'
> })
> const files: string[] = []
> for await (const item of findResult) {
>   if (item.type === 'line') files.push(item.line)
> }

> console.log(`Found ${files.length} files`)
Found 12 files

> let totalLines = 0
> for (const file of files) {
>   const wcResult = await client.bash.execute({
>     command: `wc -l "${file}"`
>   })
>   for await (const item of wcResult) {
>     if (item.type === 'line') {
>       const count = parseInt(item.line.trim().split(' ')[0])
>       if (!isNaN(count)) totalLines += count
>     }
>   }
> }

> console.log(`Total lines: ${totalLines}`)
Total lines: 3847

I found 12 TypeScript files with a total of 3,847 lines of code.

You: /save analysis.ts

Agent: Saving session to analysis.ts

Session saved to analysis.ts (18 lines)

You: /quit

$ cat analysis.ts
import { createClient } from './client.ts';

const client = await createClient();

const findResult = await client.bash.execute({
  command: 'find . -name "*.ts" -type f'
});

const files: string[] = [];
for await (const item of findResult) {
  if (item.type === 'line') files.push(item.line);
}

console.log(`Found ${files.length} files`);

let totalLines = 0;
for (const file of files) {
  const wcResult = await client.bash.execute({
    command: `wc -l "${file}"`
  });
  for await (const item of wcResult) {
    if (item.type === 'line') {
      const count = parseInt(item.line.trim().split(' ')[0]);
      if (!isNaN(count)) totalLines += count;
    }
  }
}

console.log(`Total lines: ${totalLines}`);
```

## References

- **ipython**: Python REPL with rich features (inspiration)
- **ts-node REPL**: TypeScript REPL (feature parity goal)
- **Node.js REPL**: JavaScript REPL (baseline)
- **Deno REPL**: Modern JS/TS REPL with top-level await
- **Jupyter**: Notebook interface (future inspiration)

## Relevant Code

- `src/Synapse/Transport.hs` - RPC invocation (for client calls)
- `synapse-cc` - Client generation toolchain
- `jsexec` - JavaScript execution backend
- `plexus-protocol` - Streaming protocol
