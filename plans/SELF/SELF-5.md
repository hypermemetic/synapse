---
id: SELF-5
title: "Safe write: atomic rename + mode-sensitive chmod on defaults.json"
status: Ready
type: task
blocked_by: [SELF-4]
unlocks: []
---

## Context

`defaults.json` after SELF is a manifest of credential references. Some refs (`literal:…`) still carry raw values; others (`keychain://…`, `env://…`, `file://…`) do not. The file's sensitivity depends on its contents. Write safety (atomicity) and permissions (chmod) both matter, but the policy around mode can be refined once we know whether any `literal:` refs are present.

## Goal

Every write to `defaults.json` is (a) atomic via write-to-temp + rename, and (b) chmod-enforced when the file contains literal credentials. Reads warn loudly if permissions are wider than recommended.

## Acceptance

- [ ] `Synapse.Self.writeDefaults :: Text -> StoredDefaults -> IO ()`:
  1. Create parent directory `~/.plexus/<backend>/` if missing, mode 0700.
  2. Encode `StoredDefaults` to JSON.
  3. Write to `~/.plexus/<backend>/.defaults.json.tmp`.
  4. Inspect the encoded content: if ANY value starts with `literal:`, the file is "sensitive" → chmod 0600 on the temp file before rename. Otherwise, chmod 0644 is acceptable (manifest-only; no secrets in-file).
  5. Atomic `rename` temp → `defaults.json`.
  6. On any failure, remove the temp file before propagating the error.
- [ ] Parent directory created mode 0700 when freshly created. Existing directory with wider permissions: WARN, do not chmod (user may have structure).
- [ ] `loadDefaults` checks the file's mode on Unix:
  - If the file contains any `literal:` values AND mode has group/world bits set (`mode & 0o077 != 0`): WARN naming the file and recommending `chmod 600 <path>`. Read succeeds.
  - If the file contains only non-literal refs: any mode is fine; no warning.
  - Windows: skip mode semantics (not applicable).
- [ ] Legacy tokens migration (SELF-3) writes through `writeDefaults`, inheriting the chmod policy. Since the migration produces a `literal:` entry, the migrated file ends up at 0600.
- [ ] Tests:
  - Write file with `literal:` ref → file mode 0600.
  - Write file with only `keychain://` refs → file mode 0644.
  - Simulated write failure (disk full, permission denied) → temp file cleaned up, no partial `defaults.json`.
  - Read world-readable file with `literal:` content → WARN emitted; read succeeds.
  - Read world-readable file with only `keychain://` refs → no warning.
- [ ] Windows: atomic rename still required. Permission enforcement noted as platform-gap in the module doc.

## Out of scope

- Encryption at rest (OS disk encryption is the assumed primitive).
- Password-protecting the file.
- Key-derivation from a passphrase.
- Team-shared defaults (out of scope; users use env vars / CI secrets for that, wired via `env://` refs).

## Notes

The mode policy is deliberately content-aware. A file with only `keychain://` refs is strictly a manifest — readable by anyone and still safe. Enforcing 0600 on a non-sensitive manifest is busywork without benefit. Enforcing it when `literal:` values are present keeps the security posture of the legacy tokens file without surprise over-locking.

Users who want a uniformly 0600 posture regardless of contents can set `umask 077` in their shell; the temp file respects umask before our explicit chmod.

Atomic write is non-negotiable. Half-written `defaults.json` during an abrupt kill (CTRL-C mid-`_self set`) would corrupt the store and require hand-editing.
