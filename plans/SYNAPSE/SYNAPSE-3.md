# SYNAPSE-3: Navigation (Paramorphism)

**blocked_by**: [SYNAPSE-2]
**unlocks**: [SYNAPSE-5, SYNAPSE-6]

## Scope

Implement schema navigation as a paramorphism. Navigation requires access to original substructure (child namespaces) to match path segments — this is precisely what paramorphism provides.

## Category Theory

### The Functor

```
F : Set → Set
F(X) = Text × Text × Text × Text × List(Method) × Option(List(X))
       ^^^^   ^^^^   ^^^^   ^^^^   ^^^^^^^^^^^   ^^^^^^^^^^^^^^^^
       ns     ver    desc   hash   methods       children (the hole)
```

### Why Paramorphism

**Catamorphism** receives `F(A)` — children already reduced to result type A.

**Paramorphism** receives `F(μF × A)` — children as (original, result) pairs.

Navigation must inspect child namespaces to match path segments. That information lives in μF, not in A. Hence paramorphism.

```
para : (F(μF × A) → A) → μF → A
```

## Files to Create

```
synapse/src/Synapse/Navigate.hs
```

## Core Types

```haskell
-- | The base functor for PluginSchema
data PluginF a = PluginF
  { pfNamespace   :: !Text
  , pfVersion     :: !Text
  , pfDescription :: !Text
  , pfHash        :: !Text
  , pfMethods     :: ![MethodSchema]
  , pfChildren    :: !(Maybe [a])
  }
  deriving (Functor, Foldable, Traversable)

-- | Navigation result
data NavResult
  = AtPlugin PluginSchema              -- Landed on a plugin (show help)
  | AtMethod PluginSchema MethodSchema -- Landed on a method (invoke)
  | NotFound [Text]                    -- Path segment not found
```

## Core Functions

### Paramorphism

```haskell
-- | Paramorphism over PluginSchema
--
-- At each layer, the algebra receives:
--   - The functor layer with (original, result) pairs for children
--
-- This allows inspecting original structure while computing result.
para :: (PluginF (PluginSchema, a) -> a) -> PluginSchema -> a
para alg plugin = alg $ PluginF
  { pfNamespace   = psNamespace plugin
  , pfVersion     = psVersion plugin
  , pfDescription = psDescription plugin
  , pfHash        = psHash plugin
  , pfMethods     = psMethods plugin
  , pfChildren    = fmap (map (\c -> (c, para alg c))) (psChildren plugin)
  }
```

### Navigation as Paramorphism

```haskell
-- | Navigate schema by path
--
-- This is a paramorphism because matching path segments requires
-- inspecting child namespaces (original structure), not just results.
navigate :: [Text] -> PluginSchema -> NavResult
navigate path root = para (navAlg path) root
  where
    navAlg :: [Text] -> PluginF (PluginSchema, NavResult) -> NavResult
    navAlg [] layer =
      -- Empty path: reconstruct plugin from layer
      AtPlugin (reconstruct layer)
    navAlg [name] layer
      -- Single segment: check methods first, then children
      | Just method <- findMethod name (pfMethods layer) =
          AtMethod (reconstruct layer) method
      | Just (child, _) <- findChild name (pfChildren layer) =
          AtPlugin child
      | otherwise =
          NotFound [name]
    navAlg (name:rest) layer
      -- Multiple segments: must match a child, then recurse via result
      | Just (_, childResult) <- findChild name (pfChildren layer) =
          case childResult of
            NotFound _ -> NotFound (name:rest)
            other -> other
      | otherwise =
          NotFound (name:rest)

    findMethod name = find ((== name) . msName)
    findChild name = (>>= find ((== name) . psNamespace . fst))

    reconstruct :: PluginF a -> PluginSchema
    reconstruct PluginF{..} = PluginSchema
      { psNamespace   = pfNamespace
      , psVersion     = pfVersion
      , psDescription = pfDescription
      , psHash        = pfHash
      , psMethods     = pfMethods
      , psChildren    = fmap (map fst) pfChildren  -- extract originals
      }
```

### Catamorphism (for comparison)

```haskell
-- | Catamorphism — the unique morphism from initial algebra
--
-- Less powerful than para: cannot inspect original children.
cata :: (PluginF a -> a) -> PluginSchema -> a
cata alg = para (\layer -> alg (fmap snd layer))

-- | Collect all method paths (cata suffices here)
allMethodPaths :: PluginSchema -> [[Text]]
allMethodPaths = cata collect
  where
    collect :: PluginF [[Text]] -> [[Text]]
    collect PluginF{..} =
      map (\m -> [pfNamespace, msName m]) pfMethods
      ++ concat (fromMaybe [] pfChildren)
```

## The Full Picture

```
              Rust                              Haskell

Plugin ──unfold──▶ F(Plugin)          F(μF × Result) ──para──▶ Result
    │                                          ▲
    │ ana                                      │
    ▼                                          │
   νF ──────────JSON/HTTP──────────────────▶  μF
        (serialize greatest to least)
```

## Acceptance Criteria

1. `navigate ["echo"] schema` returns `AtPlugin echoPlugin`
2. `navigate ["echo", "echo"] schema` returns `AtMethod echoPlugin echoMethod`
3. `navigate ["nonexistent"] schema` returns `NotFound ["nonexistent"]`
4. `para` correctly threads original structure through computation
5. `cata` derivable from `para` (less powerful)
6. Total functions (no partial matches)
