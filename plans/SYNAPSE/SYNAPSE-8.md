# SYNAPSE-8: JSON Schema Param Validation

**blocked_by**: [SYNAPSE-7]
**unlocks**: []

## Scope

Validate method parameters against JSON Schema before invocation. Provide clear error messages for invalid params.

## Files to Create

```
synapse/src/Synapse/Validate.hs
synapse/synapse.cabal  -- add hjsonschema or similar
```

## Dependencies to Add

```cabal
build-depends:
  , hjsonschema >= 1.10
  -- or aeson-schema, json-schema, etc.
```

## Core Functions

### Schema Validation

```haskell
-- | Validation result
data ValidationResult
  = Valid
  | Invalid [ValidationError]

data ValidationError = ValidationError
  { vePath    :: [Text]   -- JSON path to error
  , veMessage :: Text     -- Human-readable message
  }

-- | Validate params against method schema
validateParams :: MethodSchema -> Value -> ValidationResult
validateParams method params = case msParams method of
  Nothing -> Valid  -- No schema = accept anything
  Just schema -> validateAgainstSchema schema params

-- | Core JSON Schema validation
validateAgainstSchema :: Value -> Value -> ValidationResult
validateAgainstSchema schema value =
  -- Use hjsonschema or similar library
  ...
```

### Error Formatting

```haskell
-- | Format validation errors for CLI
formatValidationErrors :: [ValidationError] -> Text
formatValidationErrors errs = T.unlines $
  "Invalid parameters:" : map formatError errs

formatError :: ValidationError -> Text
formatError ValidationError{..} =
  "  " <> T.intercalate "." vePath <> ": " <> veMessage
```

### Common Validations

```haskell
-- | Check required fields
checkRequired :: Value -> [Text] -> [ValidationError]
checkRequired (Object obj) required =
  [ ValidationError [field] "Required field missing"
  | field <- required
  , not (field `member` obj)
  ]
checkRequired _ _ = []

-- | Check type constraints
checkType :: Text -> Value -> Maybe ValidationError
checkType expected value = case (expected, value) of
  ("string", String _)   -> Nothing
  ("integer", Number n)  -> if isInteger n then Nothing
                            else Just $ typeError "integer"
  ("number", Number _)   -> Nothing
  ("boolean", Bool _)    -> Nothing
  ("object", Object _)   -> Nothing
  ("array", Array _)     -> Nothing
  ("null", Null)         -> Nothing
  (exp, _) -> Just $ typeError exp
  where
    typeError exp = ValidationError [] $
      "Expected " <> exp <> ", got " <> valueType value
```

## Integration with Invoke

```haskell
-- In Synapse/Invoke.hs

invokeMethod cfg plugin method args = do
  case parseArgs method args of
    Left err -> return (Left err)
    Right params ->
      -- Validate before invoking
      case validateParams method params of
        Invalid errs -> return (Left $ formatValidationErrors errs)
        Valid -> do
          let fullMethod = psNamespace plugin <> "." <> msName method
          runConduit $ invoke cfg fullMethod params .| renderStream
          return (Right ())
```

## Error Examples

```
$ synapse echo echo
Invalid parameters:
  message: Required field missing

$ synapse echo echo --message 123
Invalid parameters:
  message: Expected string, got number

$ synapse echo echo --message "hi" --count "three"
Invalid parameters:
  count: Expected integer, got string
```

## Acceptance Criteria

1. Missing required params caught before network call
2. Wrong types caught with clear messages
3. Nested object validation works
4. Array element validation works
5. Error messages include JSON path
6. Valid params pass through unchanged
7. Schemas with no constraints accept any value
