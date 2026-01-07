# TODO: Simple String Enum Support in IR Builder

## Problem

The IR Builder at `src/Synapse/IR/Builder.hs` only partially handles simple string enums. When Rust types like `QueueStatus` and `ResourceState` serialize to JSON Schema, they use the simple enum pattern rather than a discriminated union.

### Current Behavior

In `inferTypeKind` (line 280-283), the code does attempt to handle simple enums:

```haskell
-- Check for enum (simple string enum)
| Just (Array values) <- KM.lookup "enum" o =
    let variants = [ VariantDef (asText v) Nothing [] | v <- V.toList values ]
    in KindEnum "value" variants
```

However, this creates `VariantDef` entries with empty fields, which is semantically correct for the IR but may cause issues in downstream transpilers that expect `KindEnum` to always be discriminated unions with object variants.

### The JSON Schema Pattern

Simple string enums like `QueueStatus` produce:

```json
{
  "enum": ["pending", "completed", "cancelled"],
  "type": "string"
}
```

This is distinct from discriminated unions which use `oneOf` with object variants:

```json
{
  "oneOf": [
    {
      "type": "object",
      "properties": {
        "type": { "const": "success" },
        "data": { ... }
      }
    },
    {
      "type": "object",
      "properties": {
        "type": { "const": "error" },
        "message": { ... }
      }
    }
  ]
}
```

## Expected TypeScript Output

For simple string enums:
```typescript
type QueueStatus = "pending" | "completed" | "cancelled";
```

For discriminated unions:
```typescript
type ChatEvent =
  | { type: "success"; data: Data }
  | { type: "error"; message: string };
```

## Proposed Fix

### Option 1: Add KindStringLiteral to TypeKind

Add a new `TypeKind` variant specifically for string literal unions:

```haskell
data TypeKind
  = KindStruct { ksFields :: [FieldDef] }
  | KindEnum { keDiscriminator :: Text, keVariants :: [VariantDef] }
  | KindStringLiteral { kslValues :: [Text] }  -- NEW
  | KindAlias { kaTarget :: TypeRef }
  | KindPrimitive { kpType :: Text, kpFormat :: Maybe Text }
```

Then update `inferTypeKind`:

```haskell
-- Check for enum (simple string enum)
| Just (Array values) <- KM.lookup "enum" o =
    let stringValues = [ t | String t <- V.toList values ]
    in KindStringLiteral stringValues
```

### Option 2: Use discriminator to signal simple enum

Keep using `KindEnum` but use a special discriminator value (e.g., empty string or "literal") to signal that this is a simple string enum:

```haskell
-- Check for enum (simple string enum)
| Just (Array values) <- KM.lookup "enum" o =
    let variants = [ VariantDef (asText v) Nothing [] | v <- V.toList values ]
    in KindEnum "" variants  -- Empty discriminator = string literal union
```

Transpilers would then check if `keDiscriminator == ""` to decide output format.

## Affected Types

- `QueueStatus` - changelog plugin queue entry status
- `ResourceState` - resource lifecycle states
- Any other simple Rust enums that use `#[serde(rename_all = "snake_case")]` without complex variants

## Priority

Medium - Current output is technically valid but not idiomatic. TypeScript clients would need to use object-based discrimination instead of simple string comparisons.

## Related Files

- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/IR/Builder.hs` - inferTypeKind function
- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/IR/Types.hs` - TypeKind definition
