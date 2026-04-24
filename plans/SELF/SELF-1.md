---
id: SELF-1
title: "Synapse.Self: shared defaults store module (types + file format)"
status: Pending
type: task
blocked_by: []
unlocks: [SELF-2, SELF-4]
---

## Context

synapse today reads a JWT from `~/.plexus/tokens/<backend>` as a one-off special case. It also accepts arbitrary `--cookie KEY=VALUE` and `--header KEY=VALUE` flags per invocation, but there's no way to persist those. Every run starts with an empty slate for non-JWT credentials.

We want a general per-backend **defaults store**: a single on-disk file per backend that holds default cookies and headers, read by synapse at invocation time, writable via a `_self` subcommand. The existing JWT convention collapses into this store as a single cookie default. Shared between synapse and synapse-cc via direct library import (synapse-cc already depends on `plexus-synapse`).

## Goal

Introduce a `Synapse.Self` module in the `plexus-synapse` library defining:
- The on-disk file format (one JSON file per backend)
- The in-memory `StoredDefaults` type
- Pure encode/decode functions
- The path convention

Zero IO wiring yet. Zero CLI wiring. Zero migration. This ticket establishes the contract that downstream tickets consume.

## File layout

```
~/.plexus/<backend>/defaults.json
```

## File format (JSON)

```json
{
  "version": 1,
  "defaults": {
    "cookies": {
      "access_token": "eyJ..."
    },
    "headers": {
      "X-Trace-Id": "abc"
    }
  },
  "scopes": {}
}
```

- `defaults` — backend-wide defaults applied to every method call.
- `scopes` — reserved for future per-namespace / per-method overrides (e.g. `"admin": { "cookies": {...} }` or `"admin.users.delete": {...}`). **v1 ignores this field on read**, but the schema accepts it so future tickets (SELF-7+) can land without a breaking migration.
- `version` — integer; current schema version. Decoders reject unknown versions loudly rather than silently.

## Acceptance

- [ ] New module `Synapse.Self` in `plexus-synapse` library, exported.
- [ ] `StoredDefaults` record type with `cookies :: Map Text Text`, `headers :: Map Text Text`, `scopes :: Map Text ScopedDefaults` (v1 scopes are a parsed-but-unused passthrough).
- [ ] `defaultsPath :: Text -> FilePath` — given a backend name, returns `~/.plexus/<backend>/defaults.json`.
- [ ] `encodeDefaults :: StoredDefaults -> ByteString` / `decodeDefaults :: ByteString -> Either Text StoredDefaults`. JSON, deterministic key ordering, two-space indent.
- [ ] Unknown version → `Left "unsupported version: N, expected 1"`.
- [ ] Missing `defaults` object → treated as empty (not error). Missing file is a separate concern — handled in SELF-2.
- [ ] Unit tests: roundtrip encode/decode, version-mismatch rejection, unknown-top-level-key tolerance (forward compat).
- [ ] Export list reviewed — only what SELF-2/4/6 need is public.

## Out of scope

- Reading from disk (SELF-2).
- Writing to disk (SELF-5).
- `_self` subcommand (SELF-4).
- Migration from `~/.plexus/tokens/<backend>` (SELF-3).
- Resolution precedence vs CLI flags (SELF-2).
- Scoped resolution logic (future ticket; v1 stops at the backend level).

## Notes

Keeping the scopes field in the schema on day one means the "method-level defaults" capability doesn't require a v2 migration — it just requires the resolver to start reading the field. That's the "architecture could integrate method level" part of the spec.

JSON rather than TOML because the Haskell JSON story (aeson) is already in the synapse dependency set. No new deps.

The file lives in a per-backend directory (`~/.plexus/<backend>/`) rather than a flat file, so future siblings (logs, cache, per-backend config) have a natural home.
