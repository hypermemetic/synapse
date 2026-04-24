---
id: SELF-7
title: "Core resolvers: literal, env, file"
status: Ready
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
