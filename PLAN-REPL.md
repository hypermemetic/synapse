# Synapse REPL: Session-Based Interactive Mode

## Overview

`synapse repl` provides a persistent, session-based REPL for interactive communication with backend plugins that support sessions (claudecode, cone, etc.). Unlike the current `--bidir-mode interactive` which handles individual bidirectional requests, the REPL maintains a stateful session and allows sending multiple messages through it.

## Goals

1. **Session Management**: Create, resume, fork, and delete sessions
2. **Conversational Interface**: Send multiple messages while maintaining context
3. **Plugin Agnostic**: Work with any plugin that supports sessions (claudecode, cone)
4. **Rich UX**: Meta-commands, history, streaming output, clear prompts
5. **State Preservation**: All conversation state lives in the backend session

## Architecture

### Session Lifecycle

```
┌─────────────────┐
│  synapse repl   │  1. synapse repl substrate claudecode my-session --create
└────────┬────────┘
         │
         │ 2. claudecode.create {name: "my-session", ...}
         v
┌─────────────────┐
│   substrate     │  Session created: my-session
│  (claudecode)   │
└────────┬────────┘
         │
         │ 3. REPL Loop starts
         v
┌─────────────────┐
│  synapse repl   │──> >>> write fibonacci in python
└────────┬────────┘
         │
         │ 4. claudecode.chat {name: "my-session", prompt: "write..."}
         v
┌─────────────────┐
│   substrate     │  Streams response (tokens, tool use, etc.)
└────────┬────────┘
         │
         │ 5. Display streaming response
         v
┌─────────────────┐
│  synapse repl   │──> [def fibonacci(n):...]
│                 │──>
│                 │──> >>> now test it
└────────┬────────┘
         │
         │ 6. claudecode.chat {name: "my-session", prompt: "now test it"}
         v
         ... (continues with preserved context)
```

### Command Structure

```bash
synapse repl <backend> <plugin> <session-name> [OPTIONS]

Arguments:
  <backend>        Backend identifier (substrate, plexus, etc.)
  <plugin>         Plugin name (claudecode, cone)
  <session-name>   Name for the session

Options:
  --create                     Create new session (default: true if doesn't exist)
  --no-create                  Error if session doesn't exist
  --model <MODEL>              Model to use (for claudecode: sonnet, opus, haiku)
  --working-dir <DIR>          Working directory (for claudecode)
  --system-prompt <TEXT>       System prompt (for claudecode)
  --prompt <TEXT>              Custom REPL prompt (default: ">>> ")
  --history-file <FILE>        Save command history (default: ~/.synapse_history)
  --no-history                 Disable command history
  -h, --help                   Show help message
```

## Implementation Phases

### Phase 1: Core REPL Loop

**Goal**: Basic working REPL with session management

**Tasks**:
1. [ ] Add REPL command to CLI parser
   - File: `app/Main.hs`
   - Add `ReplOpts` data type
   - Add `replParser` to argument parser
   - Wire up to main dispatch

2. [ ] Create `Synapse.REPL` module
   - File: `src/Synapse/REPL.hs`
   - `SessionInfo` type for tracking state
   - `runRepl :: ReplOpts -> SynapseM ()` main loop
   - Basic input/output handling

3. [ ] Session initialization
   - `initSession :: ReplOpts -> SynapseM SessionInfo`
   - Check if session exists (plugin-specific)
   - Create session if needed
   - Handle creation parameters (model, working_dir, etc.)

4. [ ] Message sending
   - `sendToSession :: SessionInfo -> Text -> SynapseM [StreamItem]`
   - Invoke `<plugin>.chat` method
   - Collect streaming response
   - Handle errors gracefully

5. [ ] Response display
   - `displayResponse :: [StreamItem] -> IO ()`
   - Stream tokens in real-time
   - Handle different event types (token, tool_use, done)
   - Pretty-print output

**Deliverable**: Working REPL that can create sessions and send messages

```bash
$ synapse repl substrate claudecode test --create --model sonnet
Synapse REPL - Session: test
Type /help for commands

>>> hello
Hello! How can I help you today?

>>> /quit
Goodbye!
```

### Phase 2: Meta-Commands

**Goal**: Add REPL commands for session management

**Tasks**:
1. [ ] Command parsing
   - File: `src/Synapse/REPL/Commands.hs`
   - `ReplCommand` data type
   - `parseCommand :: Text -> Maybe ReplCommand`
   - Distinguish commands from regular input

2. [ ] Implement core commands:
   - `/quit`, `/q` - Exit REPL
   - `/help`, `/h` - Show help
   - `/info` - Show session information
   - `/clear` - Clear screen

3. [ ] Session management commands:
   - `/fork <name>` - Fork current session
   - `/delete` - Delete session and exit
   - `/sessions` - List all sessions

4. [ ] Command handler
   - `handleCommand :: ReplCommand -> SessionInfo -> SynapseM CommandResult`
   - Execute command
   - Update session state if needed
   - Return whether to continue REPL loop

**Deliverable**: Full command set for session control

```bash
>>> /info
Session: my-session
Plugin: claudecode
Backend: substrate
Model: sonnet
Messages: 5
Created: 2026-02-21 22:00:00

>>> /fork experiment
Forked session to 'experiment'
Now using session: experiment

>>> /sessions
Available sessions:
  * experiment (active)
    my-session
    previous-work
```

### Phase 3: Enhanced UX

**Goal**: Polish the user experience

**Tasks**:
1. [ ] Command history
   - File: `src/Synapse/REPL/History.hs`
   - Save commands to `~/.synapse_history`
   - Arrow keys for history navigation (requires readline)
   - `/history` command to show past commands

2. [ ] Better prompts
   - Show message count: `5 >>> `
   - Show session name in prompt: `[my-session] >>> `
   - Configurable prompt string
   - Color support (optional, detect TTY)

3. [ ] Visual feedback
   - Banner on startup
   - Progress indicators for long responses
   - Distinguish user input from system output
   - Error formatting

4. [ ] Multiline input
   - `/multi` command to enter multiline mode
   - End with empty line or Ctrl+D
   - Show continuation prompt: `... `

**Deliverable**: Polished REPL experience

```bash
$ synapse repl substrate claudecode work --model opus

╔════════════════════════════════════════╗
║   Synapse REPL v0.3.0                  ║
║   Session: work (claudecode/opus)      ║
║   Type /help for commands              ║
╚════════════════════════════════════════╝

[work] 0 >>> write a function to parse JSON

[Thinking...]
Sure! Here's a robust JSON parser...
[Streams response in real-time]

[work] 1 >>> /multi
Enter multiline input (end with empty line):
... now write tests
... use pytest
... include edge cases
...

[Sends all lines as one message]
```

### Phase 4: Plugin Abstraction

**Goal**: Make REPL work with different session plugins

**Tasks**:
1. [ ] Session plugin trait
   - File: `src/Synapse/REPL/Plugin.hs`
   - `SessionPlugin` typeclass
   - Methods: `createSession`, `sendMessage`, `forkSession`, `deleteSession`

2. [ ] ClaudeCode plugin implementation
   - File: `src/Synapse/REPL/Plugins/ClaudeCode.hs`
   - Implement `SessionPlugin` for claudecode
   - Handle claudecode-specific options (model, working_dir, loopback)
   - Parse claudecode response format

3. [ ] Cone plugin implementation
   - File: `src/Synapse/REPL/Plugins/Cone.hs`
   - Implement `SessionPlugin` for cone
   - Handle cone-specific options (identifier by_name/by_id)
   - Parse cone response format

4. [ ] Plugin auto-detection
   - Detect plugin type from backend schema
   - Select appropriate plugin implementation
   - Error if plugin doesn't support sessions

**Deliverable**: Generic REPL that works with multiple plugins

```bash
# Works with claudecode
$ synapse repl substrate claudecode my-session

# Works with cone
$ synapse repl substrate cone assistant --identifier.type by_name

# Auto-detects plugin capabilities
$ synapse repl substrate echo test
Error: Plugin 'echo' does not support sessions
Supported plugins: claudecode, cone
```

### Phase 5: Advanced Features

**Goal**: Power user features

**Tasks**:
1. [ ] Streaming bidir support
   - Handle bidirectional requests during chat
   - Show bidir prompts inline
   - Support `/approve` to approve tool use
   - Support `/deny` to deny tool use

2. [ ] Session persistence
   - Save REPL state to disk
   - Resume exactly where you left off
   - Save command history per session
   - Export session transcript

3. [ ] Scripting support
   - Read commands from file: `synapse repl ... < script.txt`
   - Batch mode: execute commands non-interactively
   - Output formatting for parsing

4. [ ] Context inspection
   - `/context` - Show current conversation tree
   - `/tools` - Show available tools
   - `/messages` - Show message history

**Deliverable**: Production-ready REPL with advanced workflows

```bash
# Script execution
$ cat automation.txt
write a fibonacci function
optimize it with memoization
write tests
/quit

$ synapse repl substrate claudecode batch < automation.txt

# Context inspection
>>> /context
Conversation tree:
  [0] User: write a fibonacci function
  [1] Assistant: Here's fibonacci...
  [2] User: optimize it
  [3] Assistant: Here's the optimized version...

>>> /tools
Available tools:
  - Read (file operations)
  - Write (file operations)
  - Bash (command execution)
```

## Data Structures

### Core Types

```haskell
-- Session information
data SessionInfo = SessionInfo
  { siBackend :: Text           -- Backend name (substrate)
  , siPlugin :: Text            -- Plugin name (claudecode, cone)
  , siName :: Text              -- Session name
  , siCreated :: UTCTime        -- When session was created
  , siMessageCount :: Int       -- Number of messages sent
  , siMetadata :: Value         -- Plugin-specific metadata
  }

-- REPL options
data ReplOpts = ReplOpts
  { replBackend :: Text
  , replPlugin :: Text
  , replSession :: Text
  , replCreate :: Bool
  , replModel :: Maybe Text
  , replWorkingDir :: Maybe Text
  , replSystemPrompt :: Maybe Text
  , replPrompt :: Maybe Text
  , replHistoryFile :: Maybe FilePath
  , replEnableHistory :: Bool
  }

-- Meta commands
data ReplCommand
  = ReplQuit
  | ReplHelp
  | ReplInfo
  | ReplFork Text
  | ReplDelete
  | ReplSessions
  | ReplHistory
  | ReplClear
  | ReplMulti
  | ReplContext
  | ReplTools

-- Command result
data CommandResult
  = Continue            -- Keep REPL running
  | Exit                -- Exit REPL
  | SwitchSession Text  -- Change to different session
```

### Plugin Abstraction

```haskell
class SessionPlugin p where
  -- Create a new session
  createSession
    :: p -> Text -> ReplOpts -> SynapseM SessionInfo

  -- Send a message to the session
  sendMessage
    :: p -> SessionInfo -> Text -> SynapseM [StreamItem]

  -- Fork the session
  forkSession
    :: p -> SessionInfo -> Text -> SynapseM SessionInfo

  -- Delete the session
  deleteSession
    :: p -> SessionInfo -> SynapseM ()

  -- Check if session exists
  sessionExists
    :: p -> Text -> SynapseM Bool

  -- List all sessions
  listSessions
    :: p -> SynapseM [Text]
```

## File Structure

```
synapse/
├── app/
│   └── Main.hs                     # Add REPL command parsing
├── src/
│   └── Synapse/
│       ├── REPL.hs                 # Main REPL module
│       ├── REPL/
│       │   ├── Commands.hs         # Command parsing and handling
│       │   ├── Display.hs          # Response display formatting
│       │   ├── History.hs          # Command history management
│       │   ├── Plugin.hs           # SessionPlugin typeclass
│       │   └── Plugins/
│       │       ├── ClaudeCode.hs   # ClaudeCode implementation
│       │       └── Cone.hs         # Cone implementation
│       └── ...
└── PLAN-REPL.md                    # This file
```

## Testing Strategy

### Unit Tests

1. **Command parsing**: Verify all meta-commands parse correctly
2. **Session lifecycle**: Create, fork, delete sessions
3. **Plugin abstraction**: Mock plugin to test REPL logic
4. **Display formatting**: Test response streaming and formatting

### Integration Tests

1. **ClaudeCode REPL**: Full REPL session with claudecode
   - Create session
   - Send multiple messages
   - Fork session
   - Verify context preservation

2. **Cone REPL**: Full REPL session with cone
   - Create cone with identifier
   - Chat with cone
   - Verify responses

3. **Error handling**: Test invalid commands, network errors, session errors

### Manual Testing

1. **User workflow**: Follow typical user flow
   ```bash
   synapse repl substrate claudecode work --create --model sonnet
   >>> implement a feature
   >>> now write tests
   >>> /fork experiment
   >>> try a different approach
   >>> /quit
   ```

2. **Edge cases**: Empty input, very long input, special characters
3. **Interruption**: Ctrl+C handling, graceful shutdown

## Dependencies

### New Dependencies

```yaml
# For readline support (command history, arrow keys)
readline: "^1.0"

# For ANSI colors (optional)
ansi-terminal: "^0.11"
```

### Existing Dependencies (already have)

- `aeson` - JSON parsing
- `text` - Text handling
- `mtl` - Monad transformers
- `streaming` - Stream processing
- `async` - Concurrency

## Migration Path

### From Current Interactive Mode

Users currently use:
```bash
synapse --bidir-mode interactive substrate interactive wizard
```

This will continue to work for one-off bidirectional methods.

New REPL is for persistent sessions:
```bash
synapse repl substrate claudecode my-session
```

**No breaking changes** - we're adding a new command, not changing existing behavior.

## Success Metrics

1. **Usability**: Can create session and chat in <5 seconds
2. **Responsiveness**: Streaming responses show tokens immediately
3. **Reliability**: Handles errors gracefully, doesn't crash on invalid input
4. **Compatibility**: Works with multiple session plugins (claudecode, cone)
5. **Feature completeness**: All meta-commands work as expected

## Future Enhancements

### Post-MVP Features

1. **Multiple simultaneous sessions**: Switch between sessions with `/use <name>`
2. **Session templates**: `synapse repl --template python-dev`
3. **Rich terminal UI**: Use `brick` for TUI with panels
4. **Voice input**: Integration with speech-to-text
5. **Session sharing**: Export/import sessions
6. **Collaborative REPL**: Multiple users in same session
7. **Agent-to-agent**: REPL between two agents

### Integration Opportunities

1. **VS Code extension**: Embed REPL in editor sidebar
2. **Web UI**: Browser-based REPL interface
3. **Slack/Discord bot**: Chat with session via messaging apps
4. **GitHub Copilot**: Use REPL as backend for IDE autocomplete

## References

### Similar Tools

- **ipython**: Python REPL with rich features (inspiration for UX)
- **ghci**: Haskell REPL (meta-commands like `:t`, `:i`)
- **redis-cli**: Simple but effective command REPL
- **openai cli**: Chat interface for GPT (similar use case)

### Relevant Code

- `src/Synapse/Bidir.hs` - Current bidirectional handling
- `app/Main.hs` - CLI parsing and dispatch
- `src/Synapse/Transport.hs` - RPC invocation
- Examples in `/workspace/hypermemetic/generated-test/test/bidir-smoke.test.ts`
