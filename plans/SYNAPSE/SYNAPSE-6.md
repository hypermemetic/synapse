# SYNAPSE-6: Method Invocation + Streaming

**blocked_by**: [SYNAPSE-3, SYNAPSE-4]
**unlocks**: [SYNAPSE-7]

## Scope

Combine navigation with transport to invoke methods and stream output. Parse CLI arguments into method params, invoke via SSE, render streamed items.

## Files to Create

```
synapse/src/Synapse/Invoke.hs
```

## Core Functions

### Argument Parsing

```haskell
-- | Parse CLI args into JSON params using method schema
parseArgs :: MethodSchema -> [Text] -> Either Text Value
parseArgs method args = do
  -- Extract --key value pairs
  -- Validate against msParams schema
  -- Return as JSON object
  ...

-- | Extract expected params from JSON Schema
extractParamDefs :: Maybe Value -> [(Text, ParamDef)]
extractParamDefs schema =
  -- Parse JSON Schema properties
  -- Extract name, type, required, default
  ...

data ParamDef = ParamDef
  { pdName     :: Text
  , pdType     :: Text      -- "string", "integer", "boolean", etc.
  , pdRequired :: Bool
  , pdDefault  :: Maybe Value
  , pdDesc     :: Maybe Text
  }
```

### Method Invocation

```haskell
-- | Full method invocation
invokeMethod
  :: TransportConfig
  -> PluginSchema      -- Parent plugin
  -> MethodSchema      -- Target method
  -> [Text]            -- CLI args
  -> IO (Either Text ())
invokeMethod cfg plugin method args = do
  case parseArgs method args of
    Left err -> return (Left err)
    Right params -> do
      let fullMethod = psNamespace plugin <> "." <> msName method
      runConduit $ invoke cfg fullMethod params .| renderStream

-- | Render stream items to stdout
renderStream :: ConduitT StreamItem Void IO ()
renderStream = awaitForever $ \item -> liftIO $ case item of
  StreamData _ kind payload ->
    -- Render payload (format depends on kind)
    putStrLn $ renderPayload kind payload
  StreamProgress _ msg pct ->
    -- Render progress bar or message
    hPutStr stderr $ renderProgress msg pct
  StreamError _ msg detail _ ->
    -- Render error in red
    hPutStrLn stderr $ "Error: " <> msg
  StreamDone _ ->
    -- Clean exit
    return ()
```

### Output Rendering

```haskell
-- | Render payload based on kind
renderPayload :: Text -> Value -> Text
renderPayload kind payload = case kind of
  "echo" -> case payload of
    String s -> s
    _ -> T.pack (show payload)
  _ -> T.pack $ encodePretty payload

-- | Render progress indicator
renderProgress :: Text -> Maybe Float -> Text
renderProgress msg Nothing = msg <> "..."
renderProgress msg (Just pct) =
  msg <> " [" <> progressBar pct <> "] " <> showPct pct

progressBar :: Float -> Text
progressBar pct =
  let filled = round (pct * 20)
      empty = 20 - filled
  in T.replicate filled "█" <> T.replicate empty "░"
```

## Streaming Behavior

- Data items printed immediately (no buffering)
- Progress items update in place (carriage return)
- Errors printed to stderr
- Done terminates cleanly

## Acceptance Criteria

1. `synapse echo echo --message "hi" --count 3` streams 3 "hi" responses
2. Missing required params show clear error with expected params
3. Invalid param types show clear error
4. Progress updates render in place
5. Errors render to stderr in red
6. Output is unbuffered (real-time streaming)
