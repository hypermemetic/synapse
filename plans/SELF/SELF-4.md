---
id: SELF-4
title: "`_self` subcommand: show / set / unset / clear / import-token / upgrade-to-keychain / resolve"
status: Complete
type: task
blocked_by: [SELF-1]
unlocks: [SELF-5]
---

## Context

The defaults store is opaque without an inspection + edit surface. `_self` is the subcommand tree that exposes it.

The URI-based storage model (SELF-1) means users need two workflows: (a) "set a value" — the common case, where they just paste a JWT or token — and (b) "set a reference" — where they explicitly store `keychain://…` or `env://…`. The CLI should make both ergonomic without hiding the underlying URI model.

## Goal

Introduce `_self` for managing `~/.plexus/<backend>/defaults.json` with full visibility into stored URIs, their resolved values, and credential lifecycle (expiry, posture).

## Surface

```
synapse _self <backend> show [--json]
    → prints effective defaults as a table. For each entry:
      * the key, the stored URI, and the resolved value
      * for resolved values that look like JWTs: decoded summary
        (alg, kid, iss, aud, sub, exp with human-relative rendering)
      * explicit indication of resolution source (keychain/env/file/literal)
      * resolution failures shown inline with the ResolveError reason
      * JWT signatures NEVER printed

synapse _self <backend> set cookie <name> <value-or-uri>
synapse _self <backend> set header <name> <value-or-uri>
    → if <value-or-uri> matches a known scheme (literal:, keychain://,
      env://, file://), stored as-is.
      Otherwise wrapped as literal:<value>.
      Supports "-" to read from stdin.

synapse _self <backend> set-from-stdin cookie <name>
synapse _self <backend> set-from-stdin header <name>
    → reads entire stdin, stores as literal:<value>. Explicit form
      for when value might accidentally start with "scheme:".

synapse _self <backend> set-secret cookie <name>
synapse _self <backend> set-secret header <name>
    → reads stdin, pushes to OS keychain (service=plexus,
      account=<backend>/<kind>/<name>), stores keychain://<…> reference.
      Requires SELF-8.

synapse _self <backend> unset cookie <name>
synapse _self <backend> unset header <name>
    → removes the entry. Also offers to delete keychain items
      if the removed ref was keychain:// (prompt; --yes to skip).

synapse _self <backend> resolve cookie <name>
synapse _self <backend> resolve header <name>
    → prints the URI + resolved value + JWT summary (if applicable).
      Debugging aid: identifies exactly which resolver ran.

synapse _self <backend> upgrade-to-keychain [cookie|header] [<name>]
    → for literal: refs, push the value into keychain, rewrite the
      ref as keychain://. Applies to all entries or a single named
      one. Requires SELF-8.

synapse _self <backend> clear [--yes]
    → deletes defaults.json entirely. Prompts for confirmation
      unless --yes. Does NOT touch keychain items (use unset for that).

synapse _self <backend> import-token <path-or-->
    → convenience for the common case. Reads a JWT from file or
      stdin. By default stores as literal: (preserves behavior).
      With --to-keychain, pushes to keychain first.
```

## Acceptance

- [ ] New `_self` top-level subcommand tree in synapse's option parser. Dispatch handles the verbs above.
- [ ] `show` output is human-readable (and `--json` form for programmatic use). JWT summaries label each claim clearly; expiry renders as "expired 16d ago" or "valid for 4m 32s"; signatures never printed.
- [ ] `show` indicates the resolver for each value: `keychain` / `env` / `file` / `literal` — with the raw URI adjacent.
- [ ] `show` surfaces `ResolveError`s inline with the URI that failed and the scheme-specific reason: "env var USCIS_JWT not set" / "keychain item uscis/access_token not found".
- [ ] `set` with heuristic: auto-literal unless the value parses as a known scheme. `set-from-stdin` bypasses the heuristic.
- [ ] `set-secret` + `upgrade-to-keychain` gated on SELF-8 availability; emit a friendly error if keychain resolver isn't registered on the current platform.
- [ ] All write operations route through `Synapse.Self.writeDefaults` (SELF-5 semantics: atomic, chmod 600).
- [ ] `resolve` prints the resolved value to stdout, error to stderr; non-zero exit on failure.
- [ ] Help text accessible via `synapse _self <backend> --help`, `synapse _self <backend> <verb> --help`.
- [ ] Integration test: end-to-end round trip `set → show → resolve → unset → clear` against a tmp HOME.
- [ ] `_self` also available in synapse-cc (SELF-6 surfaces it via shared command handlers).

## Out of scope

- Atomic-write + chmod (SELF-5).
- Migrating legacy tokens file (SELF-3).
- Scoped defaults commands (namespace / method level) — v1 is backend-level.
- Interactive login / token fetching — the user has scoped this out entirely.

## Notes

`set` heuristic: a string matches "known scheme" if it begins with `literal:`, `env://`, `file://`, `keychain://`, `vault://`, or any registered scheme. Bare strings become `literal:…`. This keeps the common case (paste a JWT, call it done) frictionless while not hiding the URI model.

The `upgrade-to-keychain` command is the bridge for users who start with the migrated-from-tokens-file `literal:` refs and want better security posture. One command turns `literal:<jwt>` into `keychain://…` with the JWT pushed into the OS keychain.

Consider ordering: `show` is the highest-value command day one. Every incident (like the recent InvalidSignature one) becomes "run `_self show`, see the expired JWT flagged, refresh it" instead of a multi-hour debug session.

## Verdict (2026-04-24)

Landed as `Synapse.Self.Command` with every verb wired through a single `runSelfCommand :: SelfCommand -> IO ExitCode` entry point. The library-side ADT + handler split means SELF-6 (`synapse-cc _self`) gets the full surface for free — no duplication.

Highlights:
- `show` renders a two-section (Cookies / Headers) human-readable table, inlines the JWT summary (alg, kid, iss, aud, sub, exp with "expired 16d 4h ago" prose, iat, preferred_username, email) for any resolved value that's JWT-shaped, and surfaces `ResolveError`s inline with the failing URI + reason. Signatures are never printed — not even in `--json`.
- `set` heuristic is a static prefix list (`knownSchemes`); SELF-4 can't consult the runtime registry because SELF-8's keychain resolver wires in dynamically.
- `set-secret` / `upgrade-to-keychain` / `import-token --to-keychain` all route to one `keychainUnavailable` helper that prints a consistent "SELF-8 pending" message and exits with code 2 (distinct from the generic-error code 1).
- `writeDefaults` landed as a stub in `Synapse.Self.IO` (plain `BS.writeFile` + `createDirectoryIfMissing`); SELF-5 will tighten to atomic + chmod 600 in place without changing the call sites.
- `app/Main.hs` routing uses a legacy-subcommand allowlist (`template` / `scan` / `debug` / `validate` / `test` / `--help` / `-h`). Everything else under `_self` routes to the SELF-4 parser, which lets `synapse _self <backend>` (no verb) fall into the new help surface naturally.

Tests: 92 `self-test` examples, 30 new. Covers the set-value heuristic, JWT decode (incl. `aud` as array and padding-stripped URL-safe base64), `renderRelativeExpiry` prose, keychain-gated exit codes, and a full `set → show → resolve → unset → clear` round-trip against a temp HOME.
