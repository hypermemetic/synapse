# SYNAPSE-5: Help Rendering

**blocked_by**: [SYNAPSE-3]
**unlocks**: [SYNAPSE-7]

## Scope

Render help text for plugins and methods. When navigation lands on a plugin, show its children and methods. When it lands on a method, show usage with parameters.

## Files to Create

```
synapse/src/Synapse/Render.hs
```

## Core Functions

### Plugin Help

```haskell
-- | Render help for a plugin
renderPluginHelp :: PluginSchema -> Text
renderPluginHelp plugin = T.unlines
  [ psDescription plugin
  , ""
  , "COMMANDS:"
  , renderChildren plugin
  , renderMethods plugin
  ]

renderChildren :: PluginSchema -> Text
renderChildren plugin = case psChildren plugin of
  Nothing -> ""
  Just kids -> T.unlines $ map renderChildLine kids

renderChildLine :: PluginSchema -> Text
renderChildLine child =
  "  " <> psNamespace child <> "  " <> psDescription child

renderMethods :: PluginSchema -> Text
renderMethods plugin = T.unlines $ map renderMethodLine (psMethods plugin)

renderMethodLine :: MethodSchema -> Text
renderMethodLine method =
  "  " <> msName method <> "  " <> msDescription method
```

### Method Help

```haskell
-- | Render help for a method
renderMethodHelp :: PluginSchema -> MethodSchema -> Text
renderMethodHelp plugin method = T.unlines
  [ msDescription method
  , ""
  , "USAGE:"
  , "  synapse " <> psNamespace plugin <> " " <> msName method <> " [OPTIONS]"
  , ""
  , "OPTIONS:"
  , renderParams (msParams method)
  ]

-- | Render params from JSON Schema
renderParams :: Maybe Value -> Text
renderParams Nothing = "  (no parameters)"
renderParams (Just schema) =
  -- Parse JSON Schema properties and render as CLI flags
  -- --param-name <type>  description
  ...
```

### Error Rendering

```haskell
-- | Render navigation failure
renderNotFound :: [Text] -> PluginSchema -> Text
renderNotFound path root = T.unlines
  [ "Error: Unknown command '" <> T.intercalate " " path <> "'"
  , ""
  , "Available commands:"
  , renderPluginHelp root
  ]
```

## Output Example

```
$ synapse
Plexus MCP server - provides access to all registered activations.

COMMANDS:
  echo    Echo messages back
  health  Health check utilities

  call    Route a call to a registered activation
  hash    Get plexus configuration hash
  schema  Get full plexus schema

$ synapse echo
Echo messages back

COMMANDS:
  echo  Echo a message back
  once  Echo a simple message once

$ synapse echo echo --help
Echo a message back

USAGE:
  synapse echo echo [OPTIONS]

OPTIONS:
  --message <string>  The message to echo (required)
  --count <integer>   Number of times to repeat (default: 1)
```

## Acceptance Criteria

1. Plugin help shows children and methods
2. Method help shows usage and parameters
3. Parameters extracted from JSON Schema
4. Required vs optional params distinguished
5. Default values shown where available
