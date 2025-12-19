Often I will say "open this" in reference to code use `codium --goto` to take me there

if you need to see the dir structure use the `tre` command

## Architecture Documentation Naming Convention

Architecture documents in `docs/architecture/` use reverse-chronological naming to ensure newest documents appear first in alphabetical sorting.

**Naming formula**: `(u64::MAX - nanotime)_title.md`

Where:
- `nanotime` = current Unix timestamp in nanoseconds
- This creates a descending numeric prefix (newer = smaller number = sorts first)
- Example: `16681577588676290559_type-system.md`

**To generate a filename**:
```python
import time
nanotime = int(time.time() * 1_000_000_000)
filename = (2**64 - 1) - nanotime
print(f'{filename}_your-title.md')
```

This "chronological bubbling" helps prioritize recent architectural decisions.

## Plexus RPC Examples

The plexus server runs on `ws://localhost:4444`. Use `websocat` to interact with it.

### List all activations
```bash
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_schema","params":[]}'; sleep 1) | websocat ws://localhost:4444
```

### Get activation schema (method enum style)
```bash
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_activation_schema","params":["bash"]}'; sleep 1) | websocat ws://localhost:4444
```

### Get full schema with params and return types
```bash
(echo '{"jsonrpc":"2.0","id":1,"method":"plexus_full_schema","params":["bash"]}'; sleep 1) | websocat ws://localhost:4444
```

### Call a method (bash execute example)
```bash
# Named params
(echo '{"jsonrpc":"2.0","id":1,"method":"bash_execute","params":{"command":"echo hello"}}'; sleep 2) | websocat ws://localhost:4444

# Positional params
(echo '{"jsonrpc":"2.0","id":1,"method":"bash_execute","params":["ls -la"]}'; sleep 2) | websocat ws://localhost:4444
```

### Interactive session
```bash
websocat ws://localhost:4444
# Then paste JSON-RPC calls manually
```