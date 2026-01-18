# Template Rendering: Current State & Challenges

## Summary

The cone rendering issue has been partially resolved. We fixed the **IR type namespace collision** that caused templates to use wrong type definitions. However, **nested struct rendering** still shows Haskell's `Show` output instead of properly expanded fields.

## Completed Work

### 1. IR Type Namespace Qualification

**Problem**: Multiple plugins could define types with the same name (e.g., both `cone` and `claudecode` define `ListResult`). The IR stored types in a flat `Map Text TypeDef`, causing collisions.

**Solution**: All type names are now namespace-qualified:
- `ListResult` → `cone.ListResult` or `claudecode.ListResult`
- `ConeInfo` → `cone.ConeInfo`
- `Position` → `cone.Position`

**Files changed**:
- `src/Synapse/IR/Builder.hs` - All extraction functions now take `namespace` parameter and prefix type names

### 2. Template Generation Defaults

**Change**: `--generate-templates` now writes to `~/.config/synapse/templates/` by default instead of `.substrate/templates/`.

### 3. Array Struct Expansion

**Implemented**: Template generator now recursively expands nested struct fields for arrays:

```mustache
{{! Before }}
{{#cones}}
  head={{head}}
{{/cones}}

{{! After }}
{{#cones}}
  head=(node_id={{head.node_id}} tree_id={{head.tree_id}})
{{/cones}}
```

## Current Challenge: Mustache Dot Notation

### Symptom

Template has correct expansion:
```mustache
head=(node_id={{head.node_id}} tree_id={{head.tree_id}})
```

But output shows Haskell's `Show` representation:
```
head=fromList [("tree_id","416b5068-..."),("node_id","f7286cb9-...")]
```

### Root Cause Investigation

The rendering pipeline:
1. Raw JSON: `{"head": {"node_id": "xxx", "tree_id": "yyy"}}`
2. `wrapDiscriminatedUnion` wraps for variant sections
3. `toMustache` converts aeson `Value` to mustache's value type
4. `substituteValue` renders template

The issue appears to be in step 3 or 4. The `toMustache` function from the `mustache` library may not be preserving nested object traversal, or path resolution like `{{head.node_id}}` isn't working as expected within array iteration contexts.

### Possible Causes

1. **Mustache library behavior**: The `stache` or `mustache` Haskell libraries may handle nested path resolution differently than expected

2. **Context scoping**: Inside `{{#cones}}...{{/cones}}`, the context might not support nested object traversal via dot notation

3. **toMustache conversion**: The `ToMustache` instance for aeson's `Value` might convert nested objects to something that doesn't support path lookup

### Potential Solutions

1. **Flatten nested structs in JSON before rendering**: Transform `{"head": {"node_id": "x"}}` to `{"head.node_id": "x"}` before passing to mustache

2. **Use different mustache library**: Try `stache` which may have better nested object support

3. **Custom ToMustache instance**: Implement custom conversion that preserves nested traversal

4. **Pre-process template variables**: Replace `{{head.node_id}}` with something the library can handle

## IR Architecture

The IR is produced via **hylomorphism** - fusing unfold (schema coalgebra) with fold (IR algebra):

```haskell
buildIR :: Path -> SynapseM IR
buildIR = walkSchema irAlgebra  -- hyloM irAlgebra schemaCoalgebra
```

This means:
- No intermediate tree is materialized
- Schema is fetched lazily as needed
- Types and methods are collected in a single traversal

## File Summary

| File | Changes |
|------|---------|
| `src/Synapse/IR/Builder.hs` | Namespace-qualify all type names |
| `src/Synapse/CLI/Template.hs` | Recursive struct expansion in arrays |
| `app/Main.hs` | Default template output to `~/.config/synapse/templates/` |
| `hub-synapse.cabal` | Added `filepath` dependency to executable |

## Next Steps

1. Debug mustache variable resolution to understand why dot notation fails
2. Consider pre-flattening nested objects before mustache rendering
3. Evaluate alternative mustache libraries
4. Add tests for template rendering with nested structures
