---
id: SELF-4
title: "`_self` subcommand: show / set / unset / clear / import-token"
status: Pending
type: task
blocked_by: [SELF-1]
unlocks: [SELF-5]
---

## Context

Stored defaults are useless if you can only read them. Users need a first-class way to inspect and edit the on-disk state without reaching for a text editor. The `_self` namespace is synapse's "this CLI's view of itself talking to a backend" surface — a natural home for manipulating the local defaults store.

Debugging incidents like the recent InvalidSignature — where a 16-day-old expired token was silently resent 7 times per generation — becomes trivial if `synapse _self uscis show` prints the effective cookies/headers with decoded claim summaries for any JWTs.

## Goal

Introduce the `_self` subcommand tree for managing `~/.plexus/<backend>/defaults.json`.

## Surface

```
synapse _self <backend> show
    → prints effective defaults; for values that parse as JWTs, also
      prints summary: iss, aud, sub, exp (with "expired N days ago"
      or "valid for N hours" marker). Does NOT print raw signatures.

synapse _self <backend> set cookie <name> <value>
synapse _self <backend> set header <name> <value>
    → atomically updates defaults.json. Creates the file if missing.
      Reads <value> from stdin if passed as "-".

synapse _self <backend> unset cookie <name>
synapse _self <backend> unset header <name>
    → removes the named entry; no-op if already absent.

synapse _self <backend> clear
    → deletes defaults.json entirely (not just empties it). Prompts
      for confirmation unless --yes is passed.

synapse _self <backend> import-token <path>
    → reads a JWT from the given file (or stdin if "-"), stores it as
      cookies.access_token. Convenience for the common case.
```

## Acceptance

- [ ] New top-level subcommand `_self` in synapse's CLI option parser. Subcommand dispatch handles the verbs above.
- [ ] `show` output is human-readable. Cookies and headers are printed as a table. JWT summaries are clearly labeled and MUST NOT print the signature bytes; only header claims (`alg`, `kid`) and payload claims (`iss`, `aud`, `sub`, `exp`, `iat`, `sid`, `preferred_username`, `email` if present).
- [ ] For JWT `exp`: print both the raw timestamp AND a human-relative interpretation (`expired 16d ago` / `valid for 4m 32s`).
- [ ] `set cookie` / `set header` / `unset cookie` / `unset header` / `clear` / `import-token` all write through `Synapse.Self.writeDefaults` (lands in SELF-5 with atomic + chmod 600).
- [ ] Errors printed to stderr with clear messages, non-zero exit code. Malformed defaults file blocks read/write with pointer to the file path.
- [ ] Help text for each subcommand accessible via `synapse _self <backend> --help` etc.
- [ ] Integration test: end-to-end `set` → `show` → `unset` → `clear` round trip against a tmp HOME.
- [ ] `_self` also works with synapse-cc (SELF-6 surfaces this).

## Out of scope

- Atomic-write + chmod semantics (SELF-5).
- Migrating legacy token file (SELF-3).
- Scoped defaults (namespace / method) — `set` at namespace/method level is a future extension; v1 is backend-level only.
- Interactive "login" flow (the user explicitly scoped this out: "I don't need synapse to know how to fetch the token, just how to store it").

## Notes

`_self` naming intentionally mirrors `_info`, `_meta` etc. — underscore-prefix methods that are about the CLI/backend relationship rather than the application schema.

The JWT decoding for `show` is small: split on `.`, base64-decode payload, JSON-parse, filter allowed claims. Do NOT verify the signature (we have no key). Presentation-only.

Consider a `--json` flag on `show` for programmatic consumption. Low priority; note it if not done.
