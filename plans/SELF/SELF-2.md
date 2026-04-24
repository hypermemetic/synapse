---
id: SELF-2
title: "Read path: load defaults, dispatch URI resolution, merge with CLI flags"
status: Complete
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

## Verdict (2026-04-24)

Complete. `Synapse.Self.IO` lands `loadDefaults` / `resolveAll` / `merge` / `ResolvedDefaults` / `MethodPath` / `Cookies` / `Headers`; re-exported from `Synapse.Self`. Transport wiring consolidated into a single `buildEnv` helper in `app/Main.hs` that subsumes the three prior `initEnv + withRequestContext` sites. The helper threads the full SELF-2 pipeline — `loadDefaults backend` → `resolveAll defaultRegistry stored []` → `merge` with CLI `--cookie` / `--header` / `SYNAPSE_COOKIE_*` / `SYNAPSE_HEADER_*` entries. The legacy `--token` / `--token-file` / `~/.plexus/tokens/<backend>` path is preserved by wrapping the resolved JWT as an `access_token` CLI cookie pre-merge, so it still wins over any stored default and flows through `Transport.mergeUpgradeHeaders` unchanged. Resolve failures abort the invocation with a clear message naming the URI and underlying `ResolveError`.

Tests extended in `test/SelfSpec.hs`: 15 new specs across `loadDefaults`, `resolveAll`, and `merge` describe-blocks (covering missing file → empty, malformed JSON IOError with path, unknown version, literal-only happy path, unknown scheme short-circuit, `env://` with unset var reporting `ResolveNotFound`, no-partial-resolution invariant, `MethodPath` threading, and the four CLI-override / disjoint-union / empty-map merge cases). `cabal test plexus-synapse:self-test` passes 62/62; regression pass on `bidir-test` (51) and `parse-test` (5). Implementation did not require any refactor of `Synapse.Transport` itself — the merged cookie+header set flows through the existing `mergeUpgradeHeaders` code as-is.

Out-of-scope items (legacy `~/.plexus/tokens/<backend>` consolidation, `defaults.json` writer, `_self` subcommand, scope-level resolution, optional refs) remain deferred to SELF-3 / SELF-5 / SELF-4 / future work per the ticket.
