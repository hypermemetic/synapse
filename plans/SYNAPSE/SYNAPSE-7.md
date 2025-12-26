# SYNAPSE-7: CLI Main + Arg Parsing

**blocked_by**: [SYNAPSE-5, SYNAPSE-6]
**unlocks**: [SYNAPSE-8]

## Scope

Wire everything together into the main CLI entry point. Parse command line, fetch schema, navigate, and either show help or invoke method.

## Files to Create

```
synapse/app/Main.hs
```

## CLI Structure

```
synapse [OPTIONS] [COMMAND...]

OPTIONS:
  --url URL      Substrate URL (default: http://localhost:3000)
  --timeout SEC  Request timeout (default: 30)
  --help         Show this help
  --version      Show version

COMMAND:
  Path to plugin or method (e.g., "echo echo --message hi")
```

## Main Flow

```haskell
main :: IO ()
main = do
  opts <- parseOptions

  -- Fetch schema
  cfg <- mkTransportConfig opts
  result <- fetchSchema cfg
  schema <- case result of
    Left err -> die $ "Failed to fetch schema: " <> show err
    Right s -> return s

  -- Navigate to target
  case navigate (optPath opts) schema of
    AtPlugin plugin ->
      -- Show plugin help
      T.putStrLn $ renderPluginHelp plugin

    AtMethod plugin method
      | optHelp opts ->
          -- Show method help
          T.putStrLn $ renderMethodHelp plugin method
      | otherwise -> do
          -- Invoke method
          result <- invokeMethod cfg plugin method (optArgs opts)
          case result of
            Left err -> die $ T.unpack err
            Right () -> return ()

    NotFound path ->
      die $ T.unpack $ renderNotFound path schema
```

## Options Parsing (optparse-applicative)

```haskell
data Options = Options
  { optUrl     :: Text
  , optTimeout :: Int
  , optHelp    :: Bool
  , optVersion :: Bool
  , optPath    :: [Text]    -- Command path
  , optArgs    :: [Text]    -- Remaining args for method
  }

parseOptions :: IO Options
parseOptions = execParser $ info (optParser <**> helper)
  ( fullDesc
  <> header "synapse - CLI for Plexus schema coalgebra"
  <> progDesc "Navigate and invoke Plexus methods"
  )

optParser :: Parser Options
optParser = Options
  <$> strOption (long "url" <> value "http://localhost:3000" <> help "Substrate URL")
  <*> option auto (long "timeout" <> value 30 <> help "Timeout in seconds")
  <*> switch (long "help" <> short 'h' <> help "Show help")
  <*> switch (long "version" <> help "Show version")
  <*> many (argument str (metavar "COMMAND..."))
  <*> pure []  -- Args extracted from COMMAND after "--"
```

## Argument Splitting

The tricky part: distinguish command path from method args.

```
synapse echo echo --message hi --count 3
        ^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^
        path      args (after method found)
```

Strategy:
1. Consume args until we hit a flag (`--*`) or exhaust args
2. Everything before is path, everything after is method args
3. Or use `--` as explicit separator

```haskell
splitPathArgs :: [Text] -> ([Text], [Text])
splitPathArgs args =
  let (path, rest) = span (not . T.isPrefixOf "--") args
  in (path, rest)
```

## Acceptance Criteria

1. `synapse` with no args shows root help
2. `synapse echo` shows echo plugin help
3. `synapse echo echo --help` shows echo method help
4. `synapse echo echo --message hi` invokes and streams
5. `synapse --url http://other:3000 echo echo --message hi` uses custom URL
6. `synapse nonexistent` shows error with available commands
7. Exit codes: 0 for success, 1 for errors
