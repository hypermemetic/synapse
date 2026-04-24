---
id: SELF-1
title: "Synapse.Self: shared defaults store module (types, file format, resolver registry)"
status: Complete
type: task
blocked_by: []
unlocks: [SELF-2, SELF-4, SELF-7]
---

## Context

synapse today reads a JWT from `~/.plexus/tokens/<backend>` as a one-off special case, in plaintext, inheriting the user's umask. We're generalizing this into a per-backend defaults store for cookies and headers — AND we're making the on-disk file a **manifest of credential references**, not a plaintext credential store.

Values in the file are URIs (`keychain://…`, `env://…`, `file://…`, `vault://…`, `literal:…` as escape hatch) that a resolver registry turns into concrete values at dispatch time. This lets the same config shape support dev laptops (OS keychain), CI (env vars), and production (vault/cloud secret manager) without file format divergence.

Shared between synapse and synapse-cc via direct library import (`plexus-synapse` is already a synapse-cc dep; SELF-6 closes the loop).

## Goal

Introduce the `Synapse.Self` module family defining:
- The on-disk file format (one JSON file per backend, values are URIs)
- The in-memory `StoredDefaults` / `CredentialRef` types
- The `Resolver` typeclass and `ResolverRegistry` for scheme dispatch
- Pure encode/decode functions
- The path convention

No IO wiring yet, no CLI wiring, no actual resolvers — those come in SELF-2 (read path), SELF-4 (CLI), SELF-7 (core resolvers), SELF-8 (keychain).

## File layout

```
~/.plexus/<backend>/defaults.json
```

## File format

```json
{
  "version": 1,
  "defaults": {
    "cookies": {
      "access_token": "keychain://uscis/access_token"
    },
    "headers": {
      "X-Trace-Id": "literal:abc123",
      "X-API-Key": "env://USCIS_API_KEY"
    }
  },
  "scopes": {}
}
```

- `defaults.cookies` / `defaults.headers` — map of name → credential-reference URI.
- `scopes` — reserved for future per-namespace / per-method overrides; v1 parsed-but-ignored.
- `version` — integer; unknown versions rejected loudly.

## URI scheme contract

Every stored value is one of:

| Form | Example | Meaning |
|---|---|---|
| Opaque | `literal:<rawValue>` | Value verbatim after `literal:`. No URL-encoding; JWTs / base64 fit cleanly. |
| Hierarchical | `scheme://authority/path[?query]` | Scheme determines resolution; interpretation is scheme-specific. |

Schemes reserved in v1:
- `literal:` — opaque, escape hatch
- `env://<VAR>` — environment variable
- `file://<path>` — file contents, trailing whitespace trimmed
- `keychain://<service>/<account>` — OS credential store
- `vault://`, `aws-secrets://`, `gcp-secret://` — reserved; not implemented until there's a concrete user

Parsing: split on first `:`; if the remainder begins with `//`, parse as hierarchical URI; otherwise treat as opaque. Invalid schemes surface at resolve-time, not parse-time — the file parses as long as the shape is right.

## Types

```haskell
-- Synapse.Self.Types
data StoredDefaults = StoredDefaults
  { sdVersion :: Int
  , sdCookies :: Map Text CredentialRef
  , sdHeaders :: Map Text CredentialRef
  , sdScopes  :: Map Text ScopedDefaults   -- parsed, v1 unused
  }

newtype CredentialRef = CredentialRef { unCredentialRef :: Text }
  -- raw URI; parsed on demand by the resolver registry

-- Synapse.Self.Resolve
class Resolver r where
  scheme  :: Proxy r -> Text                                 -- e.g. "keychain"
  resolve :: r -> ParsedUri -> IO (Either ResolveError Text)

data ResolverRegistry = ResolverRegistry
  { schemes :: Map Text (ParsedUri -> IO (Either ResolveError Text)) }

data ResolveError
  = ResolveUnknownScheme Text
  | ResolveNotFound CredentialRef      -- e.g. keychain item missing, env unset
  | ResolveBackendError CredentialRef Text
  | ResolveParseError Text
  deriving (Show)
```

## Acceptance

- [ ] `Synapse.Self.Types` exports `StoredDefaults`, `CredentialRef`, `ScopedDefaults`.
- [ ] `Synapse.Self.Resolve` exports `Resolver`, `ResolverRegistry`, `ResolveError`, `ParsedUri`, and the parsing helper `parseUri :: CredentialRef -> Either Text ParsedUri`.
- [ ] `defaultsPath :: Text -> FilePath` — returns `~/.plexus/<backend>/defaults.json`.
- [ ] `encodeDefaults :: StoredDefaults -> ByteString` / `decodeDefaults :: ByteString -> Either Text StoredDefaults`. Deterministic key order, two-space indent.
- [ ] Unknown `version` → `Left "unsupported version: N, expected 1"`.
- [ ] Missing `defaults` treated as empty (not error).
- [ ] `parseUri` handles both opaque (`literal:…`) and hierarchical (`scheme://…`) forms; returns structured `ParsedUri { scheme, opaque / authority+path / query }`.
- [ ] Unit tests:
  - Roundtrip encode/decode of a file with mixed URI schemes
  - Version mismatch rejected
  - Unknown top-level fields ignored (forward compat)
  - `parseUri` recognizes `literal:foo`, `env://VAR`, `keychain://svc/account`, `file:///abs/path`, rejects bare strings without scheme
- [ ] Empty resolver registry `mempty` compiles and returns `ResolveUnknownScheme` for every lookup. (Actual resolvers land in SELF-7/SELF-8.)
- [ ] Export list reviewed — only what SELF-2/4/7/8 need is public.

## Out of scope

- Reading defaults.json from disk (SELF-2).
- Writing defaults.json (SELF-5).
- `_self` CLI subcommand (SELF-4).
- Any concrete resolver implementation (SELF-7 core, SELF-8 keychain).
- Migration of legacy tokens file (SELF-3).
- Scoped resolution logic — scopes field is carried through but unused in v1.

## Notes

Separating types from resolvers (two modules) lets the registry accept resolvers defined outside `plexus-synapse`. A future `plexus-vault` crate could provide a `VaultResolver` without modifying the core — register via `ResolverRegistry.insert "vault" resolveVault`.

Opaque `literal:` prefers simplicity (no escaping) over URI purity. Users setting arbitrary string values don't fight URL encoding.

The `ResolveError` constructors are the operator-facing error messages that `_self resolve` (SELF-4) surfaces when a reference can't be dereferenced — "keychain item 'uscis/access_token' not found" is more actionable than "error 127."

## Verdict (2026-04-24)

Landed in commit `f06563e7`.

**Modules**
- `src/Synapse/Self/Types.hs` — `StoredDefaults`, `CredentialRef`, `ScopedDefaults`, `encodeDefaults`, `decodeDefaults`, `currentVersion`, `emptyStoredDefaults`.
- `src/Synapse/Self/Resolve.hs` — `ParsedUri` / `ParsedUriBody` / `parseUri`, `Resolver` typeclass, `ResolveFn` alias, `NamedResolver`, `ResolverRegistry` (Semigroup/Monoid), `registerResolver`, `registerNamedResolver`, `lookupResolver`, `resolveRef`, `ResolveError`.
- `src/Synapse/Self.hs` — public re-export surface, `defaultsPath :: Text -> FilePath`.

**Tests**: `test/SelfSpec.hs`, new test-suite `plexus-synapse:test:self-test`. 25 examples, 0 failures. Covers mixed-scheme roundtrip, deterministic key order, two-space indent, unknown-version rejection (exact message), forward-compat tolerance for unknown top-level + nested keys, missing-`defaults`-as-empty, `parseUri` cases (literal, env, keychain, file, query string, bare reject, empty-scheme reject), `mempty` registry returns `ResolveUnknownScheme`, parse-error surfacing via `ResolveParseError`, registered-resolver dispatch, later-registration overrides.

**Decision notes**
- `defaultsPath` returns a string literally starting with `~/` rather than expanding `$HOME`, per "no IO wiring" in ticket scope. Expansion lives in SELF-2 / SELF-5 callers.
- `ResolverRegistry` is a `newtype` over `Map Text ResolveFn` with a right-biased `Semigroup` instance so later registrations override earlier ones. Matches the open-registry pattern the ticket described.
- `Resolver` typeclass retained for consumers that want instance-based declaration, but the registry stores bare `ResolveFn`s — external packages (e.g. a future `plexus-vault`) can register without importing the typeclass.
- `parseUri` query parser is deliberately minimal (split on `&` then `=`, no URL-decoding) — adequate for v1; if RFC 3986 semantics become load-bearing we can swap in `network-uri` without breaking the `ParsedUri` shape.
