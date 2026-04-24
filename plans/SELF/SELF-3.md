---
id: SELF-3
title: "Remove ~/.plexus/tokens/<backend>; migrate on first read"
status: Pending
type: task
blocked_by: [SELF-2]
unlocks: [SELF-6]
---

## Context

Today the token resolution chain is:

```
1. --token <jwt>
2. SYNAPSE_TOKEN env
3. --token-file <path>
4. ~/.plexus/tokens/<backend>      ← legacy, single-purpose, JWT-only
```

With SELF-1 + SELF-2 in place, source #4 is redundant — a JWT in `~/.plexus/<backend>/defaults.json` under `cookies.access_token` is strictly more general. The legacy path becomes a maintenance burden (duplicate code in both synapse and synapse-cc — currently tracked as SAFE-S03) and a place where users edit a plaintext JWT today while a different mechanism picks it up tomorrow.

Remove it cleanly, with a one-shot auto-migration so existing users don't break.

## Goal

The legacy `~/.plexus/tokens/<backend>` file path is no longer read anywhere in synapse or synapse-cc. On first run after this ticket lands, if a user has the legacy file and no new defaults file, synapse migrates the JWT into the new store and deletes the legacy file.

## Acceptance

- [ ] At the start of `Synapse.Self.loadDefaults`, if `defaults.json` does not exist AND `~/.plexus/tokens/<backend>` exists:
  1. Read the legacy file.
  2. Construct a fresh `StoredDefaults` with `cookies.access_token = <legacy contents, trimmed>`.
  3. Write the new `defaults.json` (atomic write — depends on SELF-5 for chmod 600; if SELF-5 hasn't landed, write with default permissions and file a follow-up).
  4. Delete the legacy file.
  5. Log at INFO: `"migrated legacy token file ~/.plexus/tokens/<backend> to ~/.plexus/<backend>/defaults.json"`.
- [ ] If the legacy file is empty or whitespace-only, delete it silently (no empty-token migration).
- [ ] After migration, `loadDefaults` returns the newly-written defaults — the caller sees the migrated values on the very same invocation.
- [ ] All code paths in synapse that previously read `~/.plexus/tokens/<backend>` directly now route through `Synapse.Self.loadDefaults`. Grep must return zero hits for that path outside of the migration function itself.
- [ ] synapse-cc: `SynapseCC.Auth.resolveToken` (the duplicated chain from SAFE-2) is updated to use `Synapse.Self` for source #4; no direct `~/.plexus/tokens/<backend>` reads remain. Full dedup comes in SELF-6; this ticket just stops the old path from being read.
- [ ] Tests:
  - Legacy file present + no new file → migrated, legacy removed, new file present with correct `cookies.access_token`.
  - Legacy file present + new file also present → legacy file deleted untouched; new file preserved (new file wins).
  - Legacy file absent + new file absent → empty defaults returned, no file created.
  - Empty legacy file → deleted, no new file created.
- [ ] `--help` text for `--token` and `--token-file` updated to reference `defaults.json` as the persistent store.
- [ ] Closes SAFE-S03 (token-resolver dedup) effectively — after SELF-6 lands, there's a single source.

## Out of scope

- chmod 600 enforcement on the written file (SELF-5).
- Adding the `_self` subcommand (SELF-4).
- Removing SAFE-S03 the ticket itself — mark it as superseded by SELF-3/SELF-6 and close when those land.

## Notes

Deleting the legacy file on migration is deliberate. Leaving it around would be confusing (two files, which one wins?) and the `loadDefaults` logic is simpler with a single source of truth. Users who had a stale expired JWT there (like the recent InvalidSignature incident) get their now-migrated-but-still-expired cookie; SELF-4's `_self` surface lets them refresh it ergonomically.
