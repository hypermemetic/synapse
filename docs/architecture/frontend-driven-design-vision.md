# Frontend-Driven Design Vision
## Building Substrate/Plexus for the TypeScript Experience We Want

**Created:** 2026-01-19
**Status:** Vision Document
**Context:** After successfully generating and testing TypeScript clients from IR v2.0

---

## Core Thesis

**We design our backend to suit our frontend.**

Traditional approach: Build backend → Generate client → Adapt frontend to awkward API
**Our approach**: Envision ideal frontend API → Shape backend to enable it → Generate perfect client

The backend exists to serve great frontend experiences. Substrate/Plexus should be shaped by what makes beautiful, intuitive TypeScript code - not the other way around.

---

## Guiding Principles

### 1. TypeScript is the Design Language

**Principle:** If it's awkward in TypeScript, the backend is wrong.

The TypeScript client is not just a "consumer" of the backend - it's the **primary interface** that defines quality. When we design a new feature, we start by writing the TypeScript code we *wish* existed, then build the backend to support it.

**Example - What We Want:**
```typescript
// Beautiful: Fluent, composable, type-safe
const conversation = await cone.chat("Explain quantum physics")
  .withContext(previousMessages)
  .stream();

for await (const token of conversation) {
  ui.appendToken(token);
}
```

**Not This:**
```typescript
// Awkward: Nested callbacks, manual stream handling
const stream = rpc.call('cone.chat', {
  prompt: 'Explain quantum physics',
  context: serializeContext(previousMessages)
});

stream.on('data', (event) => {
  if (event.type === 'token') {
    ui.appendToken(event.content);
  }
});
```

**Action Items:**
- [ ] Design all new features in TypeScript first
- [ ] Create "dream API" documents before implementation
- [ ] Reject designs that feel awkward in TypeScript
- [ ] Use TypeScript idioms (fluent APIs, async iterators, builders)

---

### 2. Streaming Should Feel Native

**Principle:** Async iterators are the currency of real-time data.

JavaScript's `for await` is elegant. Our streaming should embrace it fully, not fight it with callbacks or observables.

**Current State:**
```typescript
// Works but verbose
for await (const event of cone.chat(...)) {
  if (event.type === 'thinking') {
    // ...
  } else if (event.type === 'token') {
    // ...
  }
}
```

**Vision:**
```typescript
// Specialized streams with clean interfaces
for await (const token of cone.chat(...).tokens()) {
  ui.append(token);  // Just tokens, filtered at type level
}

// Or access full events
for await (const event of cone.chat(...).events()) {
  match(event)
    .when('thinking', e => ui.showSpinner(e.message))
    .when('token', e => ui.append(e.content))
    .when('done', e => ui.complete());
}
```

**Backend Implications:**
- Event streams should be composable/filterable
- Type discriminators should map to TypeScript union narrowing
- Streaming methods should have "refinement" variants in IR
- Consider: Stream transformers in the protocol layer

**Action Items:**
- [ ] Design stream refinement API (`.tokens()`, `.errors()`, `.progress()`)
- [ ] Add stream composition primitives to IR
- [ ] Generate helper methods for common event filtering patterns
- [ ] Create streaming utilities library (throttle, buffer, latest)

---

### 3. State is Transparent, Not Hidden

**Principle:** The backend should expose state changes explicitly, not hide them behind queries.

Modern frontends need reactive state. Instead of forcing frontends to poll, the backend should stream state changes.

**Current Pattern (Pull-based):**
```typescript
// Poll for changes - wasteful, laggy
setInterval(async () => {
  const status = await hyperforge.status();
  updateUI(status);
}, 1000);
```

**Vision (Push-based):**
```typescript
// Subscribe to state changes - efficient, real-time
for await (const change of hyperforge.watch()) {
  match(change)
    .when('workspace_bound', e => ui.showWorkspace(e.path))
    .when('repo_synced', e => ui.updateRepo(e.name))
    .when('org_added', e => ui.addOrg(e.org));
}
```

**Backend Implications:**
- Every stateful resource should have a `.watch()` method
- State changes should be event-sourced internally
- Subscriptions should use lightweight change-only events
- Consider: Diff-based state updates for large objects

**What This Means:**

1. **Reactive by Default**
   - Every entity (Cone, Tree, Workspace) emits change events
   - Frontends subscribe to what they care about
   - No polling, no stale data

2. **Fine-Grained Subscriptions**
   ```typescript
   // Watch specific resource
   cone.watch(coneId).on('message', updateChat);

   // Watch collection
   arbor.watchTrees().on('created', addToList);

   // Watch query results
   hyperforge.repos.list({ org: 'hypermemetic' })
     .watch()
     .on('added', repo => ui.add(repo))
     .on('removed', repo => ui.remove(repo));
   ```

3. **Optimistic Updates**
   ```typescript
   // Frontend applies change immediately
   ui.optimisticUpdate(newMessage);

   // Backend confirms or rejects
   await cone.chat(newMessage);  // Will emit 'message_confirmed' or 'message_failed'
   ```

**Action Items:**
- [ ] Add `.watch()` pattern to IR schema
- [ ] Design event-sourcing for core resources
- [ ] Generate watch() methods for stateful entities
- [ ] Create subscription management utilities
- [ ] Design conflict resolution for optimistic updates

---

### 4. Composition Over Configuration

**Principle:** Small primitives that compose beat large configurable APIs.

Instead of god objects with dozens of options, provide small focused functions that chain together.

**Anti-Pattern:**
```typescript
// Monolithic, hard to extend
await cone.chat({
  prompt: 'hello',
  model: 'claude-sonnet-3-5',
  temperature: 0.7,
  maxTokens: 1000,
  systemPrompt: '...',
  context: [...],
  tools: [...],
  streaming: true,
  responseFormat: 'markdown'
});
```

**Vision:**
```typescript
// Composable pipeline
await cone.create('my-agent')
  .useModel('claude-sonnet-3-5')
  .withSystem('You are a helpful assistant')
  .withTools([webSearch, calculator])
  .temperature(0.7)
  .chat('hello')
  .stream();

// Or use defaults
await cone.get('my-agent').chat('hello').stream();
```

**Backend Implications:**
- Resources should be first-class (create, get, update, delete)
- Configuration should be incremental (builder pattern)
- Sensible defaults should eliminate most configuration
- Complex workflows emerge from composition, not configuration

**Action Items:**
- [ ] Identify primitive operations for each domain
- [ ] Design builder pattern support in codegen
- [ ] Make resources immutable (changes return new instances)
- [ ] Document composition patterns in generated docs

---

### 5. Errors Are Values, Not Exceptions

**Principle:** Make failure explicit in the type system.

JavaScript exceptions break type safety and interrupt control flow. Errors should be values that TypeScript can reason about.

**Current:**
```typescript
try {
  const result = await cone.chat('hello');
  // What errors can this throw? Unknown!
} catch (err) {
  // What type is err? any!
}
```

**Vision:**
```typescript
// Result type with discriminated union
const result = await cone.chat('hello');

if (result.ok) {
  ui.show(result.value);
} else {
  match(result.error)
    .when('RateLimited', e => ui.showRetry(e.retryAfter))
    .when('ModelUnavailable', e => ui.fallbackModel(e.availableModels))
    .when('InvalidInput', e => ui.showValidation(e.issues));
}

// Or use functional style
await cone.chat('hello')
  .map(msg => ui.show(msg))
  .mapError(err => ui.showError(err))
  .unwrap();  // Only throw if explicitly requested
```

**Backend Implications:**
- Every method should declare possible errors in schema
- Errors should be typed enum variants, not string messages
- Success and failure paths should be explicit in IR
- Consider: Railway-oriented programming in protocol

**Error Hierarchy:**
```typescript
type Result<T, E> =
  | { ok: true; value: T }
  | { ok: false; error: E };

type ConeError =
  | { type: 'RateLimited'; retryAfter: number }
  | { type: 'ModelUnavailable'; availableModels: string[] }
  | { type: 'InvalidInput'; issues: ValidationIssue[] }
  | { type: 'Unauthorized'; requiredScopes: string[] }
  | { type: 'NetworkError'; retryable: boolean };
```

**Action Items:**
- [ ] Add error schemas to IR (declare possible failures)
- [ ] Generate Result<T, E> types for all methods
- [ ] Create error handling utilities (match, map, unwrap)
- [ ] Design error recovery patterns (retry, fallback, circuit breaker)
- [ ] Document error cases in generated method docs

---

### 6. Types Should Guide, Not Constrain

**Principle:** Rich types that help you write correct code, not bureaucracy.

TypeScript's type system is powerful. Use it to make impossible states unrepresentable and correct code obvious.

**Smart Constructors:**
```typescript
// Can't create invalid state
const coneId = ConeId.parse('550e8400-...'); // Result<ConeId, ParseError>
const email = Email.from('user@example.com'); // Branded type

// Type system prevents misuse
cone.get(email);  // ❌ Compile error: expected ConeId, got Email
cone.get(coneId); // ✅ Type-safe
```

**State Machines as Types:**
```typescript
// Conversation states that can't be violated
type Conversation =
  | { state: 'idle' }
  | { state: 'thinking'; startedAt: Date }
  | { state: 'streaming'; tokens: Token[] }
  | { state: 'complete'; message: Message; duration: number }
  | { state: 'failed'; error: ConeError; retryable: boolean };

// Compiler enforces valid transitions
function transition(conv: Conversation, event: Event): Conversation {
  // TypeScript ensures we handle all states and transitions
}
```

**Phantom Types for Lifecycle:**
```typescript
// Prevent using resources in wrong lifecycle state
type Cone<State> = { ... };

type Created = { readonly brand: unique symbol };
type Configured = { readonly brand: unique symbol };
type Active = { readonly brand: unique symbol };

// Can only chat with active cones
function chat(cone: Cone<Active>, msg: string): Promise<Response>;

const cone = await Cone.create('test');        // Cone<Created>
const configured = cone.withModel('sonnet');   // Cone<Configured>
const active = await configured.activate();    // Cone<Active>

await chat(cone);        // ❌ Compile error: Cone<Created> not assignable to Cone<Active>
await chat(active);      // ✅ Type-safe
```

**Backend Implications:**
- ID types should be branded (distinct from raw strings/UUIDs)
- Resource states should map to type-level states
- Invalid transitions should be compile errors, not runtime errors
- Schemas should encode constraints (min/max, patterns, enums)

**Action Items:**
- [ ] Generate branded types for IDs
- [ ] Design phantom type support for resource lifecycles
- [ ] Add type-level state machines to IR
- [ ] Generate smart constructors for constrained types
- [ ] Document type-driven design patterns

---

### 7. Discovery Should Be Ergonomic

**Principle:** IntelliSense should teach the API.

Developers should learn the API through autocomplete, not documentation.

**Rich JSDoc:**
```typescript
/**
 * Chat with an LLM agent
 *
 * @example
 * ```typescript
 * const response = await cone.chat("Explain TypeScript");
 * console.log(response.message);
 * ```
 *
 * @example Streaming
 * ```typescript
 * for await (const token of cone.chat("hello").stream()) {
 *   process.stdout.write(token);
 * }
 * ```
 *
 * @param message - The user message to send
 * @returns A Promise that resolves to the agent's response
 *
 * @throws {RateLimitError} When you exceed the rate limit
 * @throws {ModelUnavailableError} When the model is unavailable
 *
 * @see {@link https://docs.example.com/cone-chat | Documentation}
 */
chat(message: string): Promise<Response>;
```

**Type Hints:**
```typescript
// Union types with descriptions
type Model =
  | 'claude-opus-4'      // Most capable, best for complex tasks
  | 'claude-sonnet-3-5'  // Balanced performance and cost
  | 'claude-haiku-3-5';  // Fastest, most economical

// Const assertions for known values
const MODELS = {
  OPUS: 'claude-opus-4',
  SONNET: 'claude-sonnet-3-5',
  HAIKU: 'claude-haiku-3-5'
} as const;
```

**Backend Implications:**
- Schema descriptions should be rich and example-driven
- IR should carry usage examples
- Generated code should include extensive JSDoc
- Common patterns should have named exports (constants, helpers)

**Action Items:**
- [ ] Add example field to IR method schemas
- [ ] Generate JSDoc with examples for all public methods
- [ ] Create constants for enum values
- [ ] Add "See also" links in generated docs
- [ ] Generate TypeDoc-compatible documentation

---

## Domain-Specific Design Patterns

### Pattern 1: Conversational Agents (Cones)

**Vision:**
```typescript
// Create and configure
const agent = await Cone.create('research-assistant')
  .useModel('claude-opus-4')
  .withSystem('You are a research assistant')
  .withTools([webSearch, readPaper, citationLookup])
  .temperature(0.3);

// One-shot interaction
const answer = await agent.ask('What is quantum entanglement?');

// Conversational (maintains context)
const conversation = agent.conversation();
await conversation.say('Hello');
await conversation.say('What did I just say?'); // Agent remembers

// Streaming with reflection
for await (const event of agent.chat('Explain...').stream()) {
  match(event)
    .when('thinking', e => ui.showThought(e.thought))
    .when('tool_use', e => ui.showTool(e.tool, e.input))
    .when('tool_result', e => ui.showResult(e.result))
    .when('text', e => ui.appendText(e.content));
}
```

**Backend Design:**
- Cones are stateful resources with explicit lifecycle
- `.ask()` is stateless (no memory)
- `.conversation()` creates a scoped context with memory
- Streaming exposes internal reasoning (thinking, tool use)
- Tools are first-class, registerable, composable

### Pattern 2: Conversation Trees (Arbor)

**Vision:**
```typescript
// Create tree and add nodes
const tree = await Arbor.createTree({ name: 'My Conversation' });
const userMsg = await tree.addMessage({ role: 'user', content: 'Hello' });
const aiMsg = await tree.addMessage({ role: 'assistant', content: 'Hi!' }, { parent: userMsg });

// Branch at any point
const alt1 = await tree.addMessage({ role: 'user', content: 'Tell me more' }, { parent: userMsg });
const alt2 = await tree.addMessage({ role: 'user', content: 'Different question' }, { parent: userMsg });

// Query and navigate
const path = await tree.getPath(aiMsg);  // [root, userMsg, aiMsg]
const siblings = await tree.getSiblings(aiMsg);  // [alt1, alt2]

// Watch for changes
for await (const change of tree.watch()) {
  match(change)
    .when('node_added', e => ui.addNode(e.node))
    .when('head_moved', e => ui.highlightPath(e.newHead));
}

// Integrate with Cone
const cone = await Cone.get('my-agent');
await cone.chatAtNode(tree, aiMsg, 'Follow-up question');
```

**Backend Design:**
- Trees are append-only (immutable history)
- Nodes are content-addressed (hash-based)
- Navigation is explicit (parent, children, siblings, path)
- "Head" pointer for current position
- Changes stream in real-time
- Tight integration with Cones

### Pattern 3: Multi-Forge Infrastructure (Hyperforge)

**Vision:**
```typescript
// Workspace-centric design
const workspace = await Hyperforge.loadWorkspace('./my-project');

// Declarative org/repo management
await workspace.addOrg('hypermemetic', {
  forges: ['github', 'codeberg'],
  visibility: 'public'
});

// Watch for changes
for await (const event of workspace.watch()) {
  match(event)
    .when('repo_synced', e => console.log(`✓ ${e.repo}`))
    .when('conflict', e => ui.showConflict(e.repo, e.diff));
}

// Query across forges
const repos = await workspace.repos.list({
  org: 'hypermemetic',
  filter: { hasUnpushedChanges: true }
});

// Bulk operations with progress
for await (const progress of workspace.syncAll()) {
  ui.updateProgress(progress.completed, progress.total);
}
```

**Backend Design:**
- Workspace is the root aggregator
- Orgs are collections of forges
- Repos are views across multiple forges
- Operations are declarative (describe desired state)
- Convergence happens asynchronously with progress events
- Conflicts are first-class values, not errors

---

## Implementation Roadmap

### Phase 1: Foundations (Current)
✅ TypeRef v2.0 with QualifiedName
✅ Clean TypeScript generation
✅ Nested namespace support
✅ Basic streaming with async iterators

### Phase 2: Refinements (Next)
- [ ] Result<T, E> error handling
- [ ] Branded ID types
- [ ] Stream refinement methods (`.tokens()`, `.errors()`)
- [ ] Rich JSDoc generation with examples
- [ ] Builder pattern support

### Phase 3: State & Reactivity
- [ ] `.watch()` pattern for resources
- [ ] Event sourcing for core entities
- [ ] Subscription management
- [ ] Optimistic update patterns
- [ ] Conflict resolution primitives

### Phase 4: Composition Primitives
- [ ] Resource lifecycle as types
- [ ] Fluent API generation
- [ ] Smart constructors
- [ ] Type-level state machines
- [ ] Phantom types for safety

### Phase 5: Developer Experience
- [ ] Extensive examples in generated code
- [ ] IntelliSense-driven discovery
- [ ] Type-driven documentation
- [ ] Testing utilities generation
- [ ] Debug tooling

---

## Measuring Success

We'll know we've succeeded when:

1. **TypeScript feels native**
   - No "adapt the backend" patterns needed
   - IntelliSense teaches the API
   - Common tasks are 1-2 lines

2. **Frontends are declarative**
   - Describe what you want, not how to get it
   - State management is transparent
   - Errors are handled at type level

3. **Composition is obvious**
   - Small primitives combine naturally
   - No 20-parameter method calls
   - Patterns emerge from usage

4. **Streaming is ergonomic**
   - `for await` is the default
   - Filtering/transforming is built-in
   - Backpressure just works

5. **Adoption is organic**
   - Developers prefer generated client over raw RPC
   - Community creates composition patterns
   - Frontend teams drive backend features

---

## Principles in Action: Before & After

### Example: Creating and Using an Agent

**Before (Backend-Driven):**
```typescript
// Verbose, error-prone, unclear lifecycle
const response = await fetch('http://localhost:4444', {
  method: 'POST',
  body: JSON.stringify({
    jsonrpc: '2.0',
    id: 1,
    method: 'cone.create',
    params: {
      name: 'test-agent',
      model_id: 'claude-sonnet-3-5-20241022',
      system_prompt: 'You are helpful'
    }
  })
});

const { result } = await response.json();
// What's in result? Unknown!

// Now chat...?
const ws = new WebSocket('ws://localhost:4444');
ws.send(JSON.stringify({
  jsonrpc: '2.0',
  id: 2,
  method: 'cone.chat',
  params: {
    identifier: { type: 'by_name', name: 'test-agent' },
    prompt: 'Hello'
  }
}));

ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  // Parse PlexusStreamItem manually...
  if (data.result?.content_type === 'cone.chat') {
    const innerEvent = data.result.content;
    if (innerEvent.type === 'message') {
      console.log(innerEvent.message);
    }
  }
};
```

**After (Frontend-Driven):**
```typescript
// Clear, type-safe, obvious
const agent = await Cone.create('test-agent')
  .useModel('sonnet')
  .withSystem('You are helpful');

for await (const event of agent.chat('Hello')) {
  if (event.type === 'message') {
    console.log(event.message);
  }
}

// Or simpler for non-streaming
const response = await agent.ask('Hello');
console.log(response.message);
```

**Impact:**
- 50 lines → 10 lines
- Manual JSON parsing → Automatic type inference
- Unclear errors → Typed error handling
- Imperative → Declarative
- Learn the protocol → Use the API

---

## Call to Action

This vision requires us to:

1. **Start with TypeScript** - Every feature begins as TypeScript code we want to write
2. **Iterate on ergonomics** - If it feels awkward, redesign it
3. **Generate, don't abstract** - Codegen should produce beautiful code, not wrapper layers
4. **Make types work for us** - Use TypeScript's power to prevent bugs
5. **Stream by default** - Real-time should be effortless

The backend should be invisible. The TypeScript API should be delightful.

**We design our backend to suit our frontend.**

---

## References & Inspiration

- **tRPC** - Type-safe RPC with excellent DX
- **Prisma** - Generated client that feels native
- **Zod** - Schema validation with TypeScript inference
- **Effect-TS** - Functional error handling with Result types
- **XState** - Type-safe state machines
- **Remix** - Progressive enhancement philosophy
- **Railway Oriented Programming** - Error handling as values

## Next Steps

1. Review this vision with the team
2. Choose 2-3 concrete examples to prototype
3. Iterate on generated code quality
4. Build reference implementations
5. Document patterns as they emerge

The goal: TypeScript frontends so good they make us redesign the backend. Let's build it.
