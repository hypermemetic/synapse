---
id: SELF-2
title: "Read path: load defaults, dispatch URI resolution, merge with CLI flags"
status: Ready
type: task
blocked_by: [SELF-1, SELF-7]
unlocks: [SELF-3, SELF-6]
---

## Context

SELF-1 defines the file format (URIs), types, and resolver registry. SELF-7 lands the core resolvers (`literal`, `env`, `file`). This ticket wires both into synapse's request-building path so stored defaults are loaded from disk, their URI values dispatched through the resolver registry, and the results merged with CLI `--cookie` / `--header` overrides before dispatch.

## Goal

synapse reads `~/.plexus/<backend>/defaults.json`, resolves every `CredentialRef` through the registry, merges with CLI-provided flags (CLI wins), and sends the resulting cookie + header set on the WebSocket upgrade.

## Resolution chain

Most-specific wins, per key:

```
1. --cookie / --header CLI flag          (highest — wraps as literal: internally)
2. Stored defaults, scope match           (future: method > namespace > backend)
3. Stored defaults, backend level         (v1 stops here)
4. Absent                                 (lowest)
```

After merging, every ref is resolved through the registry; resolve failures surface at dispatch time with the URI that failed + the scheme-specific reason.

## Acceptance

- [ ] `Synapse.Self.loadDefaults :: Text -> IO StoredDefaults` — reads from disk, returns empty defaults if the file is absent (NOT an error). Parse errors surface as IO exceptions with clear message + file path.
- [ ] `Synapse.Self.resolveAll :: ResolverRegistry -> StoredDefaults -> MethodPath -> IO (Either ResolveError ResolvedDefaults)` — takes the store, dispatches every `CredentialRef` through the registry, returns a `ResolvedDefaults` with concrete string values. First error surfaces with the ref that failed; no partial resolution (avoid half-auth'd requests).
- [ ] `Synapse.Self.merge :: ResolvedDefaults -> Cookies -> Headers -> (Cookies, Headers)` — pure. Merges CLI overrides onto resolved stored values; CLI wins per-key.
- [ ] The `MethodPath` is threaded through `resolveAll` so SELF-7+ can add scope matching without refactoring. v1 ignores it (backend-level only).
- [ ] `Synapse.Transport` (or equivalent request-builder) calls `loadDefaults` + `resolveAll` once per invocation, then `merge` with CLI flags, then hands the final set to the WS upgrade.
- [ ] Resolve errors DO NOT abort the invocation when the failed ref happens to be a header that isn't critical — but DO abort when a cookie marked as required-by-the-backend (e.g. the auth cookie) fails. For v1 simplicity: any resolve failure aborts with a clear error. A future "optional refs" feature can relax this.
- [ ] If stored defaults contain `cookies.access_token` resolving to a valid JWT-shaped value, the existing cookie-auth path observes it — no special-casing of the token case; it's just a named cookie default.
- [ ] CLI `--cookie KEY=VALUE` and `--header KEY=VALUE` are internally wrapped as `CredentialRef "literal:VALUE"` so the resolution + merge code is unified.
- [ ] Tests:
  - Empty defaults + CLI flags → CLI flags pass through
  - Populated defaults with `literal:` values only + no CLI flags → stored values applied after resolution
  - CLI flag with same key as a stored ref → CLI wins
  - Stored ref with unknown scheme → `ResolveUnknownScheme`, dispatch aborts
  - Stored ref with scheme present but resolution fails (e.g. env var unset) → `ResolveNotFound`, dispatch aborts, error names both the CredentialRef URI and the variable name
  - Malformed JSON → parse error surfaced clearly with file path
  - Missing file → empty defaults, no error
- [ ] Existing synapse integration tests continue to pass with their current configurations.

## Out of scope

- Migrating the legacy `~/.plexus/tokens/<backend>` file (SELF-3).
- Writing `defaults.json` (SELF-5).
- `_self` CLI subcommand (SELF-4).
- Scope-level resolution — structure is in place; implementation deferred.
- Optional/best-effort refs — v1 treats every resolve failure as fatal.

## Notes

`loadDefaults` on every invocation is fine. These are small files, read once per process. The expensive part is `resolveAll` — which for `keychain://` might prompt the user for Keychain access. That's fine for a CLI tool; it happens once per session in practice (Keychain caches the unlock decision).

Wrapping CLI flags as `literal:` refs internally keeps the merge logic trivial and uniform. No special casing of "this value came from a flag" vs "this value came from a file."
