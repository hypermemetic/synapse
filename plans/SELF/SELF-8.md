---
id: SELF-8
title: "keychain:// resolver (macOS Keychain; Linux Secret Service; Windows Credential Manager)"
status: Complete
type: task
blocked_by: [SELF-1]
unlocks: []
---

## Verdict (2026-04-24)

**COMPLETE — macOS landed; Linux/Windows stubbed; tests opt-in.**

- `Synapse.Self.Resolve.Keychain` shells out to `/usr/bin/security`. Exit code 44 → `ResolveNotFound`; other failures → `ResolveBackendError` with stderr.
- `defaultRegistry` registers `keychain` on every platform — Linux/Windows return a clear "not implemented" error so `_self show` lists `keychain://` refs without "unknown scheme" confusion.
- `Synapse.Self.Command` now fully implements:
  - `set-secret` — stores stdin in keychain under `service=plexus`, `account=<backend>/<kind>/<name>`; writes the `keychain://` ref to `defaults.json`
  - `upgrade-to-keychain` — finds every `literal:` ref (or scoped subset), pushes value to keychain, rewrites the ref. Mode flips from 0600 → 0644 once no `literal:` remains.
  - `import-token --to-keychain` — convenience wrapper over set-secret targeting `cookies.access_token`
  - `unset` — when removing a `keychain://` ref, prompts (or `--yes`) to also delete the keychain item, calling `deleteFromKeychain` for real
- Linux/Windows: `runSetSecret` and `runUpgrade` route through `keychainUnavailable` (verbose error with workaround tips). `keychainResolver` returns `ResolveBackendError` "not implemented on \<platform\>".
- Tests live in a separate, opt-in test-suite `keychain-test` gated on the `build-keychain-tests` cabal flag. Default `cabal test` does NOT touch the keychain. To run: `cabal test plexus-synapse:keychain-test -f build-keychain-tests`.
- `self-test` adjustments: removed the three "SELF-8 pending" exit-code assertions in `SelfCommandSpec.hs` (the verbs no longer always error); changed two `keychain://` references in `SelfSpec.hs` to `vault://` since `keychain` is now registered in `defaultRegistry`. 108 specs passing.
- End-to-end smoke test on macOS: set-secret → show (decoded JWT, no token bytes in output) → upgrade-to-keychain (literal flips to keychain, mode 0600 → 0644) → unset (deletes keychain item) → clear. All clean, no stuck keychain entries.

## Threat-model honesty

The implementation shells out to `security`. Items are accessible by any process running as the user via the same shell-out — there is **no per-process sandbox**. README + CLAUDE.md updated to call this out explicitly. The upgrade path to per-app ACLs would be Security framework FFI with a code-signed identity; out of scope for v1.

## Context

`keychain://<service>/<account>` is the resolver that elevates the credentials store out of plaintext into OS-managed credential storage. It's what makes `defaults.json` safe to be committed to personal dotfiles (the file contains only references; the secrets live in the OS keychain).

Three platforms, three backends, one URI form. Each backend is a platform-specific module behind a feature flag; the `keychain://` scheme dispatches to whichever is available at build time.

## Goal

A `keychain://` resolver that works on macOS out of the box; Linux via Secret Service (libsecret); Windows via Credential Manager. Surface a clean `ResolveError` when no backend is available or the requested item isn't found.

## URI form

```
keychain://<service>/<account>
```

- `service` — namespaces items; convention: `plexus` for synapse/synapse-cc usage.
- `account` — the per-item name; convention: `<backend>/<kind>/<name>` (e.g. `uscis/cookie/access_token`).

`_self set-secret` (SELF-4) constructs these URIs automatically using this convention. Users are free to set references to any `keychain://service/account` of their choice for integration with other tools.

## Per-platform implementations

### macOS (`cfg(target_os = "macos")`)

- Preferred: shell out to `security find-generic-password -s <service> -a <account> -w`. Stable, no FFI. Captures stderr; if item not found, `security` exits 44; surface as `ResolveNotFound`.
- Writes via `security add-generic-password -s <service> -a <account> -w <value>` (with `-U` to update).
- User gets a standard Keychain unlock prompt on first access per session.

### Linux (`cfg(target_os = "linux")`)

- Libsecret / DBus via Secret Service protocol. Use an existing Haskell binding if available; otherwise feature-flag as "not implemented yet — contribute a PR" rather than shipping a half-baked impl.
- Minimum viable: document the expected DBus collection / label convention; fail gracefully with a clear error until implemented.

### Windows (`cfg(target_os = "windows")`)

- Credential Manager via `credwriteW` / `credreadW`. Same "ship-later-if-demand-materializes" posture as Linux.

## Acceptance

- [ ] `Synapse.Self.Resolve.Keychain` module, `keychainResolver :: Resolver`, registered under scheme `"keychain"` in `defaultRegistry` IF the current platform has a working backend; otherwise registered as a stub that returns `ResolveBackendError "keychain resolver not available on this platform"`.
- [ ] macOS implementation:
  - `resolve` via `security find-generic-password -s -a -w`
  - `store` function (exposed to SELF-4's `set-secret` / `upgrade-to-keychain`) via `security add-generic-password -U`
  - `delete` function for `_self unset` of a keychain ref
- [ ] Linux + Windows: stub resolvers that return `ResolveBackendError` with a clear "not implemented; PR welcome" message. Mark as TODO in the module doc.
- [ ] Unit tests (macOS only, gated):
  - Store + resolve round-trip
  - Resolve non-existent item → `ResolveNotFound`
  - Delete clears the item; subsequent resolve → `ResolveNotFound`
- [ ] Integration test: `_self set-secret cookie access_token < jwt.txt && _self show` → shows the `keychain://` URI and the resolved JWT summary.
- [ ] `_self unset` of a `keychain://` ref prompts "Also delete keychain item? [Y/n]" by default; `--delete-from-keychain` / `--no-delete-from-keychain` flags override.
- [ ] Clear error when someone tries `_self set-secret` on Linux/Windows before a backend is implemented.

## Out of scope

- HashiCorp Vault (`vault://`) — separate ticket when a concrete user materializes.
- AWS Secrets Manager / GCP Secret Manager — same.
- `cmd://<base64-encoded-shell>` — explicitly NOT added. Shell-exec resolvers are a security footgun; we'd rather users pick a structured resolver.
- Session caching / prompt suppression — rely on OS-level Keychain unlock behavior.

## Notes

Shell-out to `security` on macOS is deliberate. An FFI binding (e.g. via `security-framework` Rust crate, or a Haskell equivalent) would be faster but adds a build dep and failure modes. CLI tools don't need the extra 5ms.

The service/account convention is not enforced — users can store `keychain://arbitrary/key` and synapse will resolve it. The convention is only for items CREATED by `_self set-secret`, for consistency.

Linux and Windows stubs ship as "platform not yet supported" rather than broken implementations. Hard-to-debug keychain failures are worse than a clear "install keychain backend X" message. When someone with the expertise needs it on Linux, the module layout makes it obvious where the implementation plugs in.
