---
id: SELF-2
title: "Read path: resolve stored defaults and merge with CLI flags"
status: Pending
type: task
blocked_by: [SELF-1]
unlocks: [SELF-3, SELF-6]
---

## Context

SELF-1 defines the file format and pure types. This ticket wires them into synapse's request-building path so that stored defaults are applied to every RPC call, with CLI `--cookie` / `--header` flags as overrides.

## Goal

synapse reads `~/.plexus/<backend>/defaults.json` at invocation time, merges its contents with the CLI-provided flags (most-specific wins), and sends the resulting cookie + header set on the WebSocket upgrade.

## Resolution chain

Most specific to least specific, per key:

```
1. --cookie / --header CLI flag        (highest)
2. Stored defaults, scope match        (future: method > namespace > backend)
3. Stored defaults, backend level      (v1 stops here)
4. Absent                              (lowest)
```

For v1 only the backend level of stored defaults exists (SELF-1). The resolver structure must pass `method_path` through so SELF-7+ can add scope matching without refactoring.

## Acceptance

- [ ] `Synapse.Self.loadDefaults :: Text -> IO StoredDefaults` — reads from disk, returns empty defaults if the file is absent (NOT an error). Parse errors surface as IO exceptions with a clear message.
- [ ] `Synapse.Self.resolve :: StoredDefaults -> MethodPath -> Cookies -> Headers -> (Cookies, Headers)` — pure. Takes stored + CLI overrides, produces the effective set for dispatch. Currently ignores `MethodPath` (backend-level only) but threads it through so future scoped resolution plugs in.
- [ ] `Synapse.Transport` (or equivalent request builder) calls `loadDefaults` once per invocation, merges with CLI-provided cookies/headers, hands the merged set to the WS upgrade request construction.
- [ ] If the stored defaults contain `cookies.access_token`, the existing "JWT as cookie" path MUST observe it. In other words, for backends using cookie-auth, users drop their JWT into `defaults.json` and everything else works unchanged. (The legacy `~/.plexus/tokens/<backend>` lookup continues to work until SELF-3 removes it.)
- [ ] `Synapse.Self.resolveToken` helper — convenience that extracts `cookies.access_token` from the stored defaults for code that today explicitly wants the JWT (e.g. if `--token` CLI flag is absent and `SYNAPSE_TOKEN` env is unset, look here before falling back to the legacy `tokens/<backend>` file).
- [ ] Tests:
  - Empty defaults + CLI flags → CLI flags pass through
  - Populated defaults + no CLI flags → stored values applied
  - Populated defaults + CLI flag with same key → CLI wins
  - Malformed JSON → parse error surfaced clearly with file path
  - Missing file → empty defaults, no error
- [ ] Existing synapse integration tests continue to pass.

## Out of scope

- Migrating the `~/.plexus/tokens/<backend>` file (SELF-3).
- Writing to `defaults.json` (SELF-5).
- `_self` CLI surface (SELF-4).
- Scoped resolution (namespace / method) — structure is in place, implementation deferred.

## Notes

`loadDefaults` on every invocation is fine — these are small files read once per process. No need for caching or watching.

Priority of the CLI override is deliberately the same as synapse's existing behavior for `--token` vs `SYNAPSE_TOKEN` vs `--token-file` — "most specific wins." Users should not find this surprising.
