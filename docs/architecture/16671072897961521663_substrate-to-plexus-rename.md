# Substrate → Plexus Rename

The protocol was originally called "Substrate" and is now called "Plexus RPC". The public-facing name and any new code should use `plexus` / `Plexus`. The following identifiers still carry the old name and should be renamed in a future pass.

## plexus-protocol: `Plexus.Client`

| Identifier | Kind | Rename to |
|---|---|---|
| `SubstrateConfig` | type | `PlexusConfig` |
| `SubstrateConnection` | type | `PlexusConnection` |
| `substrateHost` | field | `plexusHost` |
| `substratePort` | field | `plexusPort` |
| `substratePath` | field | `plexusPath` |
| `substrateBackend` | field | `plexusBackend` |
| `substrateHeaders` | field | `plexusHeaders` |
| `substrateRpc` | function | `plexusRpc` |

## synapse: `Synapse.Transport`

| Identifier | Kind | Note |
|---|---|---|
| `getConfig` | function | returns `SubstrateConfig` — rename when type renamed |

## synapse: `Synapse.Backend.Discovery`

All `SubstrateConfig { ... }` construction sites — update field names when type renamed.

## Impact

- `SubstrateConfig` is used in: `Plexus.Client`, `Plexus.Transport`, `Synapse.Transport`, `Synapse.Backend.Discovery`, `Synapse.Self.Commands`, `Synapse.Self.Debugger`, `Synapse.Self.Protocol.TestRunner`
- Rename is mechanical (search/replace) but touches ~15 files across 2 packages
- No user-visible change (internal types, not part of synapse CLI output)
- Do after any in-flight PRs that touch these files are merged
