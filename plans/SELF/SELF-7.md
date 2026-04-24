---
id: SELF-7
title: "Core resolvers: literal, env, file"
status: Complete
type: task
blocked_by: [SELF-1]
unlocks: [SELF-2]
---

## Context

SELF-1 introduces the `Resolver` typeclass + `ResolverRegistry` but registers nothing concrete. This ticket provides the three baseline resolvers every deployment needs: `literal:` (escape hatch), `env://` (CI/CD), `file://` (existing `--token-file` idiom generalized).

## Goal

Three resolvers wired into the default registry, each a ~30-line implementation. SELF-2's read path becomes functional once this lands.

## Resolvers

### `literal:<value>`

Opaque form. Everything after `literal:` is the raw value. No URL-decoding, no trimming. If someone stores `literal: foo ` they get ` foo ` (leading/trailing spaces preserved) — enables storing values that need exact byte-for-byte fidelity.

### `env://<VAR>`

Looks up environment variable `VAR`. Unset → `ResolveNotFound`. The authority part is the variable name; path / query ignored. Example: `env://USCIS_JWT`.

### `file://<path>`

Reads file contents, strips trailing `\n` (single trailing newline — preserve interior whitespace). Path supports `~` expansion. Missing file → `ResolveNotFound`. Read error (permission denied etc) → `ResolveBackendError`.

## Acceptance

- [ ] `Synapse.Self.Resolve.Literal` module, `literalResolver :: Resolver`, registered under scheme `"literal"`.
- [ ] `Synapse.Self.Resolve.Env` module, `envResolver :: Resolver`, registered under scheme `"env"`.
- [ ] `Synapse.Self.Resolve.File` module, `fileResolver :: Resolver`, registered under scheme `"file"`.
- [ ] `Synapse.Self.Resolve.defaultRegistry :: ResolverRegistry` — returns a registry with these three pre-registered. Callers extend via `insertResolver`.
- [ ] Tests per resolver:
  - `literal:foo` → `"foo"`, `literal:` (empty) → `""`, `literal: foo ` → `" foo "`
  - `env://FOO` with FOO set → value; unset → `ResolveNotFound`
  - `file:///path/to/existing` → trimmed contents; missing → `ResolveNotFound`; unreadable → `ResolveBackendError`
  - `file://~/something` expands `~`
- [ ] Integration test: `defaultRegistry` resolves a `StoredDefaults` containing one of each scheme.

## Out of scope

- `keychain://` (SELF-8).
- `vault://`, `aws-secrets://`, `gcp-secret://` — future tickets, nothing registered for them.
- Caching resolver outputs across calls (no — every dispatch re-resolves; keychain may prompt, env may change, file may rotate).

## Notes

These three cover 90% of CI/CD and dev workflows. Keychain (SELF-8) is the other 10%.

`literal:` is deliberately opaque (no `//`, no URL-encoding) to keep it as dumb as possible — the common case is "paste a JWT, don't make me think about encoding."

`env://` using the authority slot for variable names is a slight RFC stretch — authority is supposed to be host:port. But it reads right and everyone understands it. Alternatives like `env:VAR` (opaque) or `env:///VAR` (path) are uglier for no gain.

## Verdict (2026-04-24)

Landed in commit `874906df` — `feat(SELF-7): core resolvers — literal, env, file`.

Shipped:

- `Synapse.Self.Resolve.Literal` — `literalResolver :: ResolveFn`, opaque-only dispatch, body returned verbatim.
- `Synapse.Self.Resolve.Env` — `envResolver :: ResolveFn`, hierarchical dispatch via `System.Environment.lookupEnv`; unset → `ResolveNotFound`.
- `Synapse.Self.Resolve.File` — `fileResolver :: ResolveFn`, path = `authority ++ path`, `~`/`~/` expansion via `getHomeDirectory`, single trailing `\n` stripped, `isDoesNotExistError` → `ResolveNotFound`, everything else → `ResolveBackendError`.
- `Synapse.Self.Resolve.Default` — `defaultRegistry :: ResolverRegistry` pre-registers the three under `"literal"`, `"env"`, `"file"`.
- `Synapse.Self` re-exports the resolver functions and `defaultRegistry`.

Deviations from the ticket spec:

- Ticket called for `literalResolver :: Resolver` (typeclass). The SELF-1 API that landed stores bare `ResolveFn`s in the registry and keeps `Resolver` only as a convenience class for instance-style declarations (the registry itself never uses it). Shipping `ResolveFn`s matches how `registerResolver` expects them and avoids forcing a typeclass dance on every caller. The typeclass remains available for anyone who wants it.
- Shape-mismatches (e.g. `literal://foo`, `env:FOO`, `file:/x`) return `ResolveBackendError` rather than a new error constructor. The scheme is registered, so `ResolveUnknownScheme` would be wrong; `ResolveParseError` is for malformed URIs (which these are not). `ResolveBackendError` with a clear message was the best fit with the existing four-constructor taxonomy.

Tests: 23 new specs in `test/SelfSpec.hs` under `describe` blocks `literalResolver`, `envResolver`, `fileResolver`, `defaultRegistry`. Suite total 48 examples, all green. `parse-test` and `bidir-test` spot-checked — both still green.

Out of scope (untouched): SELF-2 read-path wiring, keychain (SELF-8), other backends.
