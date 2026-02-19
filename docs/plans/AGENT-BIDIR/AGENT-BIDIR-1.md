# AGENT-BIDIR-1: Making Synapse Usable to an Agent — Bidirectional

**Status:** Planning
**Date:** 2026-02-17

---

## Problem

When an AI agent (or any non-TTY process) calls a bidirectional method via synapse, it gets nothing useful:

```haskell
detectBidirMode :: IO BidirMode
detectBidirMode = do
  tty <- isTTY
  pure $ if tty then BidirInteractive else BidirAutoCancel  -- agent always hits this
```

The server sends a `StreamRequest`, synapse detects no TTY, and silently cancels. The agent never sees the request and the workflow aborts.

`BidirJson` mode exists but requires interleaved stdin/stdout on a running process — incompatible with how agents use the Bash tool (output is captured after process exit, not during).

Additionally, the type system is too loose: `handleBidirRequest :: BidirMode -> Text -> StandardRequest -> IO StandardResponse` accepts any response for any request. There's nothing stopping a handler responding to a `ConfirmRequest` with `TextResponse "sure"`.

---

## Context: The Design We Arrived At

### Current types (flat, no type safety)

```rust
// plexus-core
pub enum StandardRequest {
    Confirm { message, default },
    Prompt  { message, default, placeholder },  // response always String
    Select  { message, options, multi },
    // no escape hatch for custom types
}

pub enum StandardResponse {
    Confirmed(bool),
    Text(String),       // hardcoded — Prompt always returns String
    Selected(Vec<String>),
    Cancelled,
}
```

```haskell
-- plexus-protocol
data StandardRequest = ConfirmRequest .. | PromptRequest .. | SelectRequest ..
data StandardResponse = ConfirmedResponse Bool | TextResponse Text | SelectedResponse [Text] | CancelledResponse

handleBidirRequest :: BidirMode -> Text -> StandardRequest -> IO StandardResponse
-- ^ no link between request variant and valid response variants
```

### Proposed types: `StandardRequest<T>` / `StandardResponse<T>`

The `T` parameter serves double duty: `Prompt<T>` asks for a value of type T (not hardcoded String), and `Custom(T)` is a typed escape hatch for activations that don't fit the three standard shapes.

```rust
// plexus-core
pub enum StandardRequest<T = serde_json::Value> {
    Confirm { message: String, default: Option<bool> },
    Prompt  { message: String, default: Option<T>, placeholder: Option<String> },
    Select  { message: String, options: Vec<SelectOption<T>>, multi: bool },
    Custom(T),
}

pub enum StandardResponse<T = serde_json::Value> {
    Confirmed(bool),
    Value(T),           // replaces Text(String) — now typed to T
    Selected(Vec<T>),   // options are typed
    Custom(T),
    Cancelled,
}

pub type StandardBidirChannel = BidirChannel<StandardRequest, StandardResponse>;
// T defaults to serde_json::Value — backwards compatible
```

This gives us:

| Channel | `T` | Behaviour |
|---------|-----|-----------|
| Standard text prompt | `String` / `Value` | Current behaviour, no change |
| Typed prompt | `ProjectConfig` | Synapse renders fields from schema, validates response |
| Fully custom exchange | `FileConflict` / `MergeStrategy` | `Custom(T)` in and out |

### Agent-usable dispatch modes

Two new modes alongside the existing four:

**`--bidir-cmd <command>`** — subprocess per request:
```bash
synapse --bidir-cmd ./handle.sh substrate interactive wizard
```
For each `StreamRequest`, synapse spawns the command with the request JSON on stdin, reads the response from stdout. Each request is a discrete subprocess — no interactivity required. An agent can implement the script or point it at another synapse call.

**`--bidir-respond` / separate response call** — explicit handle:
```bash
# Call starts, pauses at bidir request, prints it structured
synapse substrate interactive wizard
# {"type":"bidir_request","request_id":"abc","request":{"type":"prompt","message":"Enter name",...}}

# Agent responds via separate call
synapse substrate respond --request-id abc --type value --value "my-project"
# stream resumes
```

This maps to how agents use tools — discrete calls, not long-running interactive processes.

### Schema-driven bidir types in the IR

The IR builder currently drops the `Arc<BidirChannel<Req, Resp>>` parameter. With this design it emits the `T` type:

```json
{
  "mdName": "wizard",
  "mdBidirType": { "$ref": "#/defs/ProjectConfig" }
}
```

`null` for `T = serde_json::Value` (standard behavior). A `TypeRef` for typed channels. Synapse's dispatch uses the same IR-driven field rendering it already uses for method params.

---

## Dependency DAG

```
AGENT-BIDIR-2 (StandardRequest<T> / StandardResponse<T> in plexus-core)
      │
      ├──> AGENT-BIDIR-3 (plexus-protocol Haskell types to match)
      │          │
      │          └──> AGENT-BIDIR-4 (Synapse typed dispatch — Bidir.hs)
      │                      │
      │          ┌───────────┼───────────┐
      │          ▼           ▼           ▼
      │    AGENT-BIDIR-5  AGENT-BIDIR-6  AGENT-BIDIR-7
      │    (--bidir-cmd)  (--respond)    (schema-driven)
      │
      └──> AGENT-BIDIR-8 (IR builder emits mdBidirType)
                 │
                 └──> AGENT-BIDIR-7 (schema-driven dispatch consumes it)
```

---

## Tickets

| Ticket | Description | Blocked By | Unlocks |
|--------|-------------|------------|---------|
| AGENT-BIDIR-2 | `StandardRequest<T>` / `StandardResponse<T>` in plexus-core | — | 3, 8 |
| AGENT-BIDIR-3 | Haskell types in plexus-protocol to match | 2 | 4 |
| AGENT-BIDIR-4 | Synapse typed dispatch — link request variant to valid response variants | 3 | 5, 6, 7 |
| AGENT-BIDIR-5 | `--bidir-cmd` subprocess mode | 4 | — |
| AGENT-BIDIR-6 | `--bidir-respond` / separate response call | 4 | — |
| AGENT-BIDIR-7 | Schema-driven bidir dispatch using `mdBidirType` | 4, 8 | — |
| AGENT-BIDIR-8 | IR builder emits `mdBidirType` from channel type params | 2 | 7 |
| AGENT-BIDIR-9 | Tests — typed dispatch, --bidir-cmd, agent end-to-end | 5, 6, 7 | — |

---

## Phase Breakdown

### Phase 1: Typed foundation (AGENT-BIDIR-2, 3, 4)

Make the type system reflect the constraint that exists semantically but not currently in code: a `ConfirmRequest` can only produce `Confirmed(bool)` or `Cancelled`, never `Value(T)`.

Rust: `StandardRequest<T>` / `StandardResponse<T>` with `T = serde_json::Value` default (backwards compat).

Haskell (`plexus-protocol`):
```haskell
data Request t
  = Confirm { message :: Text, confirmDefault :: Maybe Bool }
  | Prompt  { message :: Text, promptDefault  :: Maybe t   }
  | Select  { message :: Text, options        :: [t]       }
  | Custom t

data Response t = Value t | Selected [t] | Confirmed Bool | Cancelled

type BidirHandler = forall t. TypeRef -> Request t -> IO (Response t)
-- TypeRef is the IR type for t — drives rendering when t is a schema type
```

Dispatch bridge (keeps `StandardRequest`/`StandardResponse` as the JSON wire format):
```haskell
dispatchRequest :: BidirHandler -> StandardRequest -> IO StandardResponse
```

### Phase 2: Agent-usable modes (AGENT-BIDIR-5, 6)

**`--bidir-cmd`**: For each `StreamRequest`, spawn the command with request JSON on stdin. Read one line from stdout as the response. Validate against the expected response type before sending.

Wire format for the subprocess:
```json
// stdin
{"type":"bidir_request","request_id":"abc","variant":"prompt","message":"Enter name","default":null}

// stdout (expected)
{"type":"value","value":"my-project"}
// or: {"type":"cancelled"}
```

With typed channels, synapse knows in advance what `"value"` shape to expect and can reject malformed responses before they reach the server.

**`--bidir-respond`**: Print the request to stdout as a structured `bidir_request` item alongside normal stream output. The agent makes a separate `synapse substrate respond --request-id <id> ...` call. Synapse holds the pending channel open and unblocks it on receipt of the response.

### Phase 3: Schema-driven dispatch (AGENT-BIDIR-7, 8)

The IR builder inspects the `Arc<BidirChannel<Req, Resp>>` parameter in macro-generated method signatures and emits `mdBidirType` into `MethodDef`. Synapse's dispatch uses the same `TypeRef`-driven field rendering it already has for method output — no special cases.

For `T = serde_json::Value` (current standard usage): dispatch works as today.
For `T = ProjectConfig`: synapse renders the fields as a form (interactive) or schema (for `--bidir-cmd`).

### Phase 4: Tests (AGENT-BIDIR-9)

- Unit: typed dispatch rejects wrong response variants at compile time (Haskell)
- Unit: `--bidir-cmd` validates response JSON against expected type
- Integration: agent loop — synapse calls `interactive.wizard`, `--bidir-cmd` script responds, stream completes
- Regression: existing `StandardBidirChannel` usage unchanged

---

## Critical Path

```
AGENT-BIDIR-2 → AGENT-BIDIR-3 → AGENT-BIDIR-4 → AGENT-BIDIR-5
```

Minimum viable for agent use: typed Rust types → Haskell types → typed dispatch → `--bidir-cmd`.

---

## Open Questions

1. **`--bidir-respond` state**: where does synapse hold the pending channel between calls? In-process via a named socket / pid file, or does substrate expose a `respond` method that synapse calls?
2. **`T` in the wire format**: does `Custom(T)` need a type discriminator in the JSON, or is the schema sufficient to deserialise?
3. **Multiple outstanding requests**: can a streaming method send two `StreamRequest` items before either is answered? Current channel impl allows it — `--bidir-respond` needs to surface the request ID clearly.
4. **Backwards compat on `StandardResponse`**: `Text(String)` → `Value(T)` is a rename at the Haskell level. Existing code matching `TextResponse` breaks. Strategy: deprecation alias or one-shot rename?
