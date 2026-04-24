---
id: SELF-5
title: "Safe write: atomic rename + mode-sensitive chmod on defaults.json"
status: Complete
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

## Verdict (2026-04-24)

Landed as `feat(SELF-5): atomic write + mode-aware chmod on defaults.json`
(commit `332a1284`). `writeDefaults` now writes to a hidden sibling
`.defaults.json.tmp`, chmods `0600` iff the encoded output contains any
`literal:` substring (`0644` otherwise), then atomic-renames into place;
`bracketOnError` removes the temp file on any mid-write failure so no
partial `defaults.json` can exist. A freshly created parent dir gets
`0700`; a pre-existing wide dir triggers a stderr WARN without touching
its mode. `loadDefaults` sniffs mode on Unix after a successful read and
emits a stderr WARN (with a `chmod 600 <path>` recommendation) on
world/group-accessible files that carry `literal:` content; manifests
with only `env://` / `keychain://` / `file://` refs stay silent at any
mode. Windows skips the POSIX steps — atomic rename still happens.

Scope held exactly to the ticket. Out of scope for this ticket and
explicitly deferred: SELF-3 tokens migration, SELF-6 codegen dedup,
SELF-8 keychain integration. No keychain resolver added, no external
`security` binary invoked, no test touches the real OS keychain.

Test count: 102 self-test specs pass (92 pre-existing + 10 new).
Smoke-tested manually: `HOME=/tmp/self5smoke synapse _self testbk set
cookie access_token "literal:hello"` produces `0700` on
`~/.plexus/testbk/` and `0600` on `defaults.json`; the same flow with a
`keychain://svc/tok` value yields `0644`.
