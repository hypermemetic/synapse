---
id: SELF-8
title: "keychain:// resolver (macOS Keychain; Linux Secret Service; Windows Credential Manager)"
status: Pending
type: task
blocked_by: [SELF-1]
unlocks: []
---

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
