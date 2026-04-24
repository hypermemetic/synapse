---
id: SELF-3
title: "Remove ~/.plexus/tokens/<backend>; migrate to literal: ref on first read"
status: Complete
type: task
blocked_by: [SELF-2]
unlocks: [SELF-6]
---

## Context

The legacy `~/.plexus/tokens/<backend>` file is JWT-only, plaintext, and inherits the user's umask. SELF-1 + SELF-2 make it redundant: the same JWT can live in `defaults.json` as `cookies.access_token = literal:<jwt>` (preserves current security posture) or upgraded to `cookies.access_token = keychain://…` (better) via `_self` commands (SELF-4).

Keeping the legacy path around creates confusion (two files, which wins?) and blocks the SAFE-S03 dedup. Remove it, with a one-shot auto-migration on first read so existing users don't break.

**Security note:** We deliberately migrate to `literal:` and NOT to `keychain://`. A silent permission escalation would surprise users and might pop an OS keychain prompt unexpectedly. Preserving current posture is the right default; upgrading to keychain is a deliberate `_self` command.

## Goal

After this ticket, the string `~/.plexus/tokens/<backend>` appears nowhere in synapse or synapse-cc source except inside the migration function itself.

## Acceptance

- [ ] At the start of `Synapse.Self.loadDefaults`, if `defaults.json` does not exist AND `~/.plexus/tokens/<backend>` exists:
  1. Read the legacy file; trim leading/trailing whitespace.
  2. Construct a fresh `StoredDefaults` with `cookies.access_token = CredentialRef "literal:<jwt>"`.
  3. Write the new `defaults.json` via `writeDefaults` (SELF-5 semantics).
  4. Delete the legacy file.
  5. Log at INFO: `"migrated legacy token file ~/.plexus/tokens/<backend> to ~/.plexus/<backend>/defaults.json (stored as literal:). Consider: synapse _self <backend> upgrade-to-keychain"`.
- [ ] If the legacy file is empty or whitespace-only, delete it silently.
- [ ] After migration, `loadDefaults` returns the newly-written defaults on the same invocation — no double-work for the caller.
- [ ] All remaining direct reads of `~/.plexus/tokens/<backend>` in synapse and synapse-cc are removed. Grep returns zero hits outside the migration function.
- [ ] synapse-cc's `SynapseCC.Auth.resolveToken` updated to call `Synapse.Self.resolveToken` helper instead of reading the legacy path directly. Full dedup lands in SELF-6.
- [ ] `--help` text for `--token` and `--token-file` updated to reference `defaults.json` as the persistent store.
- [ ] Tests:
  - Legacy file present + no new file → migrated with `literal:` ref, legacy removed
  - Legacy file + new file both present → legacy deleted, new file preserved (new wins)
  - Neither file → empty defaults, no file created
  - Empty legacy file → deleted, no new file
- [ ] SAFE-S03 status updated to point at this ticket + SELF-6 as the vehicle.

## Out of scope

- chmod 600 on written file (SELF-5).
- `_self upgrade-to-keychain` subcommand (SELF-4 adds it).
- Removing the SAFE-S03 ticket itself — mark superseded by SELF-3 + SELF-6, close when both land.

## Notes

Migration to `literal:` is a conservative choice. The user can immediately run `synapse _self <bk> upgrade-to-keychain` (which pushes the resolved value into the OS keychain and rewrites the ref). That's an opt-in posture upgrade, not a surprise.

If a user's legacy token is expired (plausible — see the April InvalidSignature incident), they get the expired token migrated verbatim. `_self show` (SELF-4) then surfaces the expiry clearly, and they can refresh. Better than silently preserving bad state without visibility.

## Verdict (2026-04-24)

Complete. `Synapse.Self.IO.loadDefaults` now calls a `migrateIfNeeded` helper at the top of every invocation:

1. Legacy present, no new file → read the legacy bytes, trim whitespace, construct `StoredDefaults { sdCookies = Map.singleton "access_token" (CredentialRef "literal:<jwt>") }`, write via `writeDefaults` (SELF-5 atomic rename + 0600 chmod because `literal:` is in the encoded bytes), delete the legacy file, and log `[INFO] migrated legacy token file … to … (stored as literal:). Consider: synapse _self <backend> upgrade-to-keychain`. On the same invocation the existing body then reads the freshly-written file naturally — no double-work for the caller.
2. Legacy + new both present → legacy deleted unread; new file untouched; `[INFO] removed stale legacy token file … (superseded by defaults.json)`.
3. Empty / whitespace-only legacy → deleted silently; no new file created.
4. Neither file → no-op.

Migration target is deliberately `literal:<jwt>`, not `keychain://`, per the Notes section: silent permission escalation (and any OS keychain prompt) is an opt-in `synapse _self <backend> upgrade-to-keychain` verb (SELF-8 scope), not a side effect of first-boot.

Direct reads of `~/.plexus/tokens/<backend>` are removed from both repos. In synapse, `resolveToken` in `app/Main.hs` now only consults `--token` / `--token-file`; the persistent per-backend fallback flows through the SELF-2 pipeline inside `buildEnv`. In synapse-cc, `SynapseCC.Auth.resolveToken` delegates its final step to `Synapse.Self.loadDefaults` + `defaultRegistry` — full dedup lands with SELF-6. Help text for `--token` / `--token-file` (both repos) and the `-32001` hint in `Synapse.Transport` now reference `~/.plexus/<backend>/defaults.json` as the persistent store.

`git grep -n 'plexus/tokens'` after the change: zero hits in synapse source outside the migration function's doc/code; zero hits in synapse-cc source. All references outside that function live in `plans/` ticket files.

Tests: new `test/SelfMigrationSpec.hs` covers the full migration matrix (legacy-only → literal: migration + 0600 chmod + INFO line wording; legacy + new → stale-legacy drop with unchanged new file; neither file → empty defaults + no files created; empty / whitespace-only legacy → silent drop; idempotency across two consecutive `loadDefaults` calls). `cabal test plexus-synapse:self-test` runs 111 examples (102 pre-existing + 9 new), 0 failures.

Smoke-tested against the built `synapse` executable under `HOME=/tmp/self3smoke`: migration INFO emitted on stderr, `_self show` surfaces the migrated `literal:` ref (and decodes the embedded JWT), legacy file is gone, new `defaults.json` lands at mode `0600` with the `literal:<jwt>` body under `cookies.access_token`.
