---
id: SELF-5
title: "Safe write: atomic + chmod 600 on defaults.json"
status: Pending
type: task
blocked_by: [SELF-4]
unlocks: []
---

## Context

`defaults.json` holds session cookies and bearer tokens — reusable credential material. Today's `~/.plexus/tokens/<backend>` file inherits the user's umask (typically 644 on macOS), meaning anyone on the machine with read access to the home directory can grab the JWT. That was a latent issue for the legacy path; we should not carry it forward.

Additionally, any write sequence (read → modify → write) needs to be crash-safe. A partially-written `defaults.json` corrupts the store and blocks future reads until the user hand-edits it.

## Goal

Every write to `defaults.json` is (a) atomic via write-to-temp + rename, and (b) chmod 600 after write. Reads refuse to proceed if permissions are wider than the user expects.

## Acceptance

- [ ] `Synapse.Self.writeDefaults :: Text -> StoredDefaults -> IO ()`:
  1. Create parent directory `~/.plexus/<backend>/` if missing (mode 700).
  2. Write encoded JSON to `~/.plexus/<backend>/.defaults.json.tmp` (or similar).
  3. `chmod 600` on the temp file.
  4. `rename` temp → `defaults.json` (atomic on POSIX).
  5. On any failure, remove the temp file before propagating the error.
- [ ] Parent directory created with mode 700 (drwx------). If it already exists with wider permissions, log a WARN and continue. Do NOT chmod an existing directory — user may have deliberate structure.
- [ ] `loadDefaults` checks the file's mode on Unix. If world/group readable (mode & 0o077 != 0), log a WARN naming the file and the recommended fix (`chmod 600 <path>`). Do not refuse to read — just warn, so inherited-from-legacy files don't hard-break.
- [ ] Legacy tokens file migration (SELF-3) writes the new file through `writeDefaults`, inheriting these semantics.
- [ ] Tests:
  - Write on a fresh home: file exists with mode 600.
  - Write when parent dir exists: file has mode 600, parent dir untouched.
  - Simulated write failure (e.g. disk full): temp file cleaned up, no partial `defaults.json`.
  - Read of a world-readable file: WARN emitted, read succeeds.
- [ ] Windows: skip chmod semantics (not applicable). Atomic rename is still required. Note the permission enforcement gap in the module doc.

## Out of scope

- Encryption of the file at rest (separate concern; OS-level disk encryption is the assumed primitive).
- Key-derivation / password-protecting the file (out of scope for synapse; left to OS keychain integrations if users want it).
- Access control beyond the user: the file is owner-only; group/world excluded. Users needing team-shared defaults use env vars or CI secrets, not this file.

## Notes

The temp-file-then-rename pattern is the canonical POSIX atomic-write. On macOS and Linux this is safe across power loss (within fsync constraints — acceptable for this use case). The parent directory's mode-700 creation is defense in depth: even if someone chmod'd the file wider later, the directory still gatekeeps access.

"Warn don't refuse" on permissive-mode read is the right tradeoff for a dev tool. Users upgrading from the legacy 644 file will get one WARN per invocation until they `chmod 600`; escalating to hard-refuse would be user-hostile during the migration window.
