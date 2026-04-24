---
id: SELF-6
title: "synapse-cc integration: import Synapse.Self, delete SynapseCC.Auth, expose `synapse-cc _self`"
status: Complete
type: task
blocked_by: [SELF-2, SELF-3]
unlocks: []
---

## Context

synapse-cc already imports `plexus-synapse` as a library dependency (cabal.project: `packages: . ../synapse ...`; synapse-cc.cabal line 60: `build-depends: , plexus-synapse`). Modules like `Synapse.Monad`, `Synapse.IR.Builder`, `Synapse.Log`, `Synapse.Backend.Discovery` are already in use from `Pipeline.hs` / `Watch.hs` / `RegistryResolve.hs`.

The SAFE-2 token resolver in synapse-cc (`SynapseCC.Auth.resolveToken`) is a duplicate of synapse's own `resolveToken` — documented as a temporary dup to be cleaned up (SAFE-S03). With `Synapse.Self` in place, that dedup happens for free: both tools consume the same module.

Additionally, synapse-cc users should be able to use the `_self` surface without a separate binary — `synapse-cc _self uscis show` should behave identically to `synapse _self uscis show`.

## Goal

1. synapse-cc delegates all defaults-store reads to `Synapse.Self`. The SAFE-2 `SynapseCC.Auth.resolveToken` function either calls through or is deleted outright.
2. synapse-cc exposes a `_self` subcommand that reuses the same command logic as synapse.

## Acceptance

- [ ] `SynapseCC.Auth.resolveToken` is either:
  - (a) deleted entirely, with all callers switched to `Synapse.Self.resolveToken` (preferred), OR
  - (b) reduced to a thin alias `resolveToken = Synapse.Self.resolveToken` if any callers need the old name for backwards compat — but prefer (a).
- [ ] Zero remaining direct reads of `~/.plexus/tokens/<backend>` in synapse-cc source. Grep must return no hits.
- [ ] `synapse-cc _self <backend> <verb>` subcommand tree works identically to `synapse _self ...`. Implementation SHOULD share the command handlers — expose them from `Synapse.Self.Command` (or similar) and call from both CLIs.
- [ ] Help text for `synapse-cc` top-level `--help` lists `_self` as a subcommand.
- [ ] Existing synapse-cc integration tests pass unchanged (the resolved token contents should be identical; only the code path differs).
- [ ] Closes SAFE-S03 — mark that ticket Complete with a reference to this one.

## Out of scope

- Changing synapse-cc's pipeline shape (the IR generation flow stays the same; only the token/headers resolution shifts).
- Adding synapse-cc-specific `_self` verbs beyond what SELF-4 ships in synapse.
- Re-exporting `Synapse.Self` as a synapse-cc public module (keep it as `Synapse.Self` — it's clearer that the shared store is synapse's canonical surface).

## Notes

This ticket is the payoff for establishing the library-dependency pattern. Once it lands, adding a new credential source or a new `_self` verb is a one-crate change that both CLIs pick up automatically. The SAFE-2 duplication regret (SAFE-S03) was correctly anticipated — SELF-6 is the scheduled dedup.

If the shared command handlers feel awkward in `Synapse.Self.Command`, put them in a new `Synapse.Self.CLI` module. Either way, both CLIs call the same functions; the only per-CLI code is parsing the arguments out of the host CLI's option-parser structure.
