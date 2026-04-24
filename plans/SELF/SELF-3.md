---
id: SELF-3
title: "Remove ~/.plexus/tokens/<backend>; migrate to literal: ref on first read"
status: Ready
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
