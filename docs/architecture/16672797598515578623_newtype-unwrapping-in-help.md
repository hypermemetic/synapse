# Newtype Unwrapping in Help Display

## Problem

Rust newtypes like `PaneRef(String)` generate JSON Schema with a `$ref` pointing to a named type definition. Without intervention, synapse's help renderer displays the qualified type name:

```
--pane <panes.PaneRef?>  (optional)
```

Users see an opaque type name that gives no indication of what to actually pass.

## Schema Pipeline

A Rust newtype with `#[schemars(transparent)]`:

```rust
#[derive(JsonSchema)]
#[schemars(transparent)]
pub struct PaneRef(pub String);
```

generates this JSON Schema for a parameter using it:

```json
{
  "pane": { "$ref": "#/$defs/PaneRef" }
}
```

with the definition:

```json
{
  "$defs": {
    "PaneRef": { "type": "string" }
  }
}
```

The IR builder creates a `TypeDef` with `KindPrimitive "string" Nothing` for `PaneRef`. The help renderer then encounters `RefNamed "panes.PaneRef"` and must decide what to display.

## Without `#[schemars(transparent)]`

If the Rust newtype omits `transparent`, schemars generates:

```json
{
  "$defs": {
    "PaneRef": {
      "type": "object",
      "properties": { "0": { "type": "string" } },
      "required": ["0"]
    }
  }
}
```

This looks like a struct to the IR builder, and renders as `<any>` or the struct name. **Always use `#[schemars(transparent)]` on newtypes that should appear as their inner type in CLI help.**

## Solution: `renderTypeRef` Unwrapping

In `src/Synapse/CLI/Help.hs`, `renderTypeRef` follows named references through to their underlying type:

```haskell
renderTypeRef :: IR -> TypeRef -> Text
renderTypeRef ir = \case
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      -- Newtype over a primitive: display the primitive
      Just TypeDef{tdKind = KindPrimitive t mf}
        | Just fmt <- mf -> fmt
        | otherwise -> t
      -- Alias: follow the chain
      Just TypeDef{tdKind = KindAlias target} ->
        renderTypeRef ir target
      -- Struct/enum: display the type name (user needs to see structure)
      _ -> name
  -- ... other cases
```

This unwraps:
- `KindPrimitive` → displays the primitive type (`string`, `uint64`, etc.)
- `KindAlias` → recursively follows to the target type
- `KindStruct` / `KindEnum` → keeps the qualified name (these have structure worth naming)

## Result

```
-- Before:
--pane <panes.PaneRef?>  (optional)

-- After:
--pane <string?>  (optional)
```

## When to Use Which Pattern

| Rust type | Schema attr | IR result | Help display |
|-----------|-------------|-----------|-------------|
| `String` | — | `RefPrimitive "string"` | `<string>` |
| `PaneRef(String)` | `#[schemars(transparent)]` | `RefNamed` → `KindPrimitive "string"` | `<string>` (unwrapped) |
| `PaneRef(String)` | (none) | `RefNamed` → `KindStruct` | `<any>` (broken) |
| `ConeIdentifier` (enum) | — | `RefNamed` → `KindEnum` | `<ConeIdentifier>` (kept, has structure) |

**Rule**: If a newtype exists for type safety in Rust but is just a string/number to the CLI user, add `#[schemars(transparent)]`. If the type has internal structure the user needs to know about (enums, structs), leave it named.

## Files

- **Fix**: `src/Synapse/CLI/Help.hs` — `renderTypeRef` unwraps primitives and aliases
- **Rust side**: Use `#[schemars(transparent)]` on newtypes (e.g., `PaneRef`, `PaneId`)
