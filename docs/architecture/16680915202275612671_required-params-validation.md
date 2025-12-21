# Required Parameters Validation

## Problem

When a user omits a required parameter, they get an unhelpful server error:

```bash
$ synapse arbor node-create-text
Subscription error: RpcErrorObj {errCode = -32602, errMessage = "Invalid params",
  errData = Just (String "missing field `tree_id` at line 1 column 2")}
```

Instead, they should get a helpful CLI error:

```bash
$ synapse arbor node-create-text
Missing required option: --tree-id

Usage: synapse arbor node-create-text --tree-id UUID --content TEXT [OPTIONS]
```

## Current Architecture

```
User Input
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  optparse-applicative                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ buildTypedMethodParser                               │    │
│  │   --tree-id UUID    → optional (strOption)          │    │
│  │   --content TEXT    → optional (strOption)          │    │
│  │   --metadata JSON   → optional (strOption)          │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│              All params treated as optional                  │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
CommandInvocation { invParams = {} }   ← Empty/partial params
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│  Substrate RPC                                               │
│  Validates params, returns error if required fields missing  │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
"missing field `tree_id`"   ← Server error, not CLI error
```

## Why It's This Way

### 1. Schema Ambiguity

The enriched schema's `required` field is on the wrong level:

```json
{
  "required": ["method", "params"],    // ← Says "method" and "params" are required
  "properties": {
    "method": { "type": "string" },
    "params": {
      "properties": {
        "tree_id": { "type": "string", "format": "uuid" },
        "content": { "type": "string" }
      }
      // No "required" array here for params!
    }
  }
}
```

The `required` array lists `["method", "params"]` - indicating the RPC envelope fields are required, not which parameters within `params` are required.

### 2. Current Code Path

In `Plexus/Dynamic.hs`, `buildParamParser`:

```haskell
paramParser :: Parser (Maybe Value)
paramParser
  | paramRequired param = Just <$> requiredParser   -- Uses "required" from schema
  | otherwise = optional optionalParser
```

But `paramRequired` is always `False` because:

```haskell
-- In Schema.hs
toParamSchema :: [Text] -> (Text, SchemaProperty) -> ParamSchema
toParamSchema required (name, prop) = ParamSchema
  { paramRequired = name `elem` required   -- "required" is from parent schema
  , ...
  }

-- The "required" list comes from schemaRequired variant, which is ["method", "params"]
-- not the actual param names like ["tree_id", "content"]
```

### 3. The Root Issue

The substrate's JSON Schema structure doesn't propagate `required` into the `params` object properties. We're checking if `"tree_id" elem ["method", "params"]` which is always `False`.

## Proposed Solution

### Option A: Infer Required from Type (Recommended)

If a param is non-nullable and has no default, treat it as required:

```haskell
-- New logic in Schema.hs
isParamRequired :: SchemaProperty -> Bool
isParamRequired prop =
  not (isNullable (propType prop)) && isNothing (propDefault prop)

-- Where isNullable checks for ["string", "null"] type arrays
isNullable :: Maybe Value -> Bool
isNullable (Just (Array types)) = String "null" `elem` toList types
isNullable _ = False
```

### Option B: Look for Nested Required Array

Check if the `params` property has its own `required` array:

```haskell
extractParams :: SchemaProperty -> Maybe [Text] -> [ParamSchema]
extractParams paramsProp _mParentRequired =
  case propProperties paramsProp of
    Just props ->
      let paramsRequired = fromMaybe [] (propRequired paramsProp)  -- Use params' own required
      in map (toParamSchema paramsRequired) (Map.toList props)
    Nothing -> []
```

This requires adding `propRequired` field to `SchemaProperty`:

```haskell
data SchemaProperty = SchemaProperty
  { propType        :: Maybe Value
  , propDescription :: Maybe Text
  , propFormat      :: Maybe Text
  , propItems       :: Maybe SchemaProperty
  , propDefault     :: Maybe Value
  , propEnum        :: Maybe [Value]
  , propProperties  :: Maybe (Map Text SchemaProperty)
  , propRequired    :: Maybe [Text]   -- ← Add this
  }
```

### Option C: Substrate Fix

Update substrate to include `required` in the params schema:

```json
{
  "properties": {
    "params": {
      "required": ["tree_id", "content"],   // ← Add this
      "properties": {
        "tree_id": { ... },
        "content": { ... }
      }
    }
  }
}
```

## Recommendation

**Option A (Infer Required)** is the quickest fix and doesn't require substrate changes:

1. Non-nullable params without defaults → required flags
2. Nullable params (`["string", "null"]`) → optional flags
3. Params with defaults → optional flags

This matches user intuition: if a param can't be null and has no default, you must provide it.

## Implementation Plan

1. Add `isNullable` helper to check for nullable types
2. Update `toParamSchema` to infer `paramRequired` from nullability + default
3. Test that required params now fail at CLI level with helpful error

## Expected Result

```bash
$ synapse arbor node-create-text
Missing: --tree-id UUID --content TEXT

Usage: synapse arbor node-create-text --tree-id UUID --content TEXT
                                          [--parent UUID] [--metadata JSON]

  Create a text node in a tree

Required options:
  --tree-id UUID           UUID of the tree
  --content TEXT           Text content for the node

Optional:
  --parent UUID            UUID of the parent node
  --metadata JSON          Optional node metadata
```
