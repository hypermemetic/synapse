# SYNAPSE-1: Haskell CLI for Schema Coalgebra

## Goal

Build a Haskell CLI (`synapse`) that consumes the recursive `PluginSchema` from the Rust substrate. The CLI derives commands from schema — add a plugin, get a CLI command for free.

## Category Theory

### The Functor

```
F : Set → Set
F(X) = Text × Text × Text × Text × List(Method) × Option(List(X))
```

This is an endofunctor. The last component contains the hole.

### Fixed Points

- **μF** — initial algebra, least fixed point. Finite trees only. What we serialize.
- **νF** — final coalgebra, greatest fixed point. The actual plugin network.

### Coalgebra (Rust side)

```
unfold : Plugin → F(Plugin)
```

The pair (Plugin, unfold) is an F-coalgebra living in νF.

### Algebra (Haskell side)

```
fold : F(Result) → Result
```

The pair (Result, fold) is an F-algebra.

### The Morphisms

| Morphism | Signature | Use |
|----------|-----------|-----|
| Catamorphism | `(F(A) → A) → μF → A` | Fold from leaves to root |
| Anamorphism | `(S → F(S)) → S → νF` | Unfold from seed outward |
| Hylomorphism | `cata ∘ ana` | Unfold then fold |
| **Paramorphism** | `(F(μF × A) → A) → μF → A` | Fold with access to original structure |

### Why Paramorphism for Navigation

Navigation must inspect child namespaces to match path segments. That information lives in μF, not in A.

- Cata receives: `F(A)` — children already reduced
- Para receives: `F(μF × A)` — children as (original, result) pairs

Hence navigation is a paramorphism.

### The Bridge

```
            Rust                              Haskell

Plugin ──unfold──▶ F(Plugin)        F(μF × Result) ──para──▶ Result
    │                                        ▲
    │ ana                                    │
    ▼                                        │
   νF ──────────JSON/HTTP────────────────▶  μF
        (serialize greatest to least)
```

JSON serialization is the morphism from νF to μF.

## Key Insight

The schema carries intrinsic hashes (see `16680006406217676543_schema-intrinsic-hashing.md`). This enables:
- Lazy schema fetching with precise cache invalidation
- Method-level change detection
- True coalgebraic unfolding

## Dependency DAG

```
                SYNAPSE-2 (types)
                      │
          ┌───────────┴───────────┐
          ▼                       ▼
  SYNAPSE-3 (para)        SYNAPSE-4 (transport)
          │                       │
          ▼                       │
  SYNAPSE-5 (render)              │
          │                       │
          └───────────┬───────────┘
                      ▼
              SYNAPSE-6 (invoke)
                      │
                      ▼
              SYNAPSE-7 (cli main)
                      │
                      ▼
              SYNAPSE-8 (validation)
```

## Tickets

| ID | Title | Blocked By | Unlocks | Size |
|----|-------|------------|---------|------|
| SYNAPSE-2 | Core types (Schema, Stream, PluginF) | - | 3, 4 | S |
| SYNAPSE-3 | Paramorphism + navigation | 2 | 5, 6 | M |
| SYNAPSE-4 | HTTP/SSE transport | 2 | 6 | M |
| SYNAPSE-5 | Help rendering | 3 | 7 | S |
| SYNAPSE-6 | Method invocation + streaming | 3, 4 | 7 | M |
| SYNAPSE-7 | CLI main + arg parsing | 5, 6 | 8 | M |
| SYNAPSE-8 | JSON Schema param validation | 7 | - | M |

## Success Criteria

1. `synapse` with no args shows root plugin help
2. `synapse echo` navigates to echo plugin help
3. `synapse echo echo --message "hi" --count 3` invokes with streaming output
4. `synapse echo echo --badparam` shows error with valid params
5. Output streams in real-time (not buffered)
6. Hashes enable cache invalidation

## Non-Goals (Phase 2+)

- REPL mode
- Tab completion
- WebSocket transport
- Schema caching with disk persistence
