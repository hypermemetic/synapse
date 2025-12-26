# SYNAPSE-2: Core Types

**blocked_by**: []
**unlocks**: [SYNAPSE-3, SYNAPSE-4]

## Scope

Define the Haskell types that mirror the Rust schema and stream types. These are the foundation everything else builds on.

## Files to Create

```
synapse/
├── synapse.cabal
├── cabal.project
└── src/
    └── Synapse/
        ├── Schema.hs     -- PluginSchema, MethodSchema, PluginF
        └── Stream.hs     -- StreamItem, StreamMetadata
```

## Types

### The Base Functor

```haskell
-- | Base functor for PluginSchema
--
-- F(X) = Text × Text × Text × Text × List(Method) × Option(List(X))
--
-- Used for recursion schemes (cata, para, ana).
data PluginF a = PluginF
  { pfNamespace   :: !Text
  , pfVersion     :: !Text
  , pfDescription :: !Text
  , pfHash        :: !Text
  , pfMethods     :: ![MethodSchema]
  , pfChildren    :: !(Maybe [a])
  }
  deriving (Functor, Foldable, Traversable)
```

### PluginSchema (μF)

```haskell
-- | Fixed point of PluginF — the initial algebra
--
-- This is μF, the least fixed point. Finite trees only.
data PluginSchema = PluginSchema
  { psNamespace   :: !Text
  , psVersion     :: !Text
  , psDescription :: !Text
  , psHash        :: !Text           -- Intrinsic hash
  , psMethods     :: ![MethodSchema]
  , psChildren    :: !(Maybe [PluginSchema])
  }
```

### MethodSchema (with hash)

```haskell
data MethodSchema = MethodSchema
  { msName        :: !Text
  , msDescription :: !Text
  , msHash        :: !Text           -- Compile-time hash
  , msParams      :: !(Maybe Value)  -- JSON Schema
  , msReturns     :: !(Maybe Value)  -- JSON Schema
  }
```

### StreamItem

```haskell
data StreamItem
  = StreamData     !StreamMetadata !Text !Value
  | StreamProgress !StreamMetadata !Text !(Maybe Float)
  | StreamError    !StreamMetadata !Text !(Maybe Text) !Bool
  | StreamDone     !StreamMetadata
```

### Conversion

```haskell
-- | Project PluginSchema to one layer of PluginF
project :: PluginSchema -> PluginF PluginSchema
project PluginSchema{..} = PluginF
  { pfNamespace   = psNamespace
  , pfVersion     = psVersion
  , pfDescription = psDescription
  , pfHash        = psHash
  , pfMethods     = psMethods
  , pfChildren    = psChildren
  }

-- | Embed one layer of PluginF into PluginSchema
embed :: PluginF PluginSchema -> PluginSchema
embed PluginF{..} = PluginSchema
  { psNamespace   = pfNamespace
  , psVersion     = pfVersion
  , psDescription = pfDescription
  , psHash        = pfHash
  , psMethods     = pfMethods
  , psChildren    = pfChildren
  }
```

## Acceptance Criteria

1. Types compile with no warnings
2. JSON round-trips correctly (verified with QuickCheck or manual tests)
3. Hash fields parse from substrate schema
4. `isHub`/`isLeaf` helpers work correctly
5. `project`/`embed` form an isomorphism
6. `PluginF` is Functor, Foldable, Traversable
