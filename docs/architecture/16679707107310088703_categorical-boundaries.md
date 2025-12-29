# Categorical Boundaries: When Algebra Applies and When It Doesn't

## Introduction

Category theory provides powerful abstractions for structuring computation. But not every computation fits neatly into categorical structure. This document explains:

1. What it means to work "within" a categorical structure
2. The benefits you get from staying within that structure
3. When and why computations exit the categorical world
4. How to design systems that maximize time spent in structured computation

## The Core Idea

A **category** consists of:
- Objects (types, in programming)
- Morphisms (functions between types)
- Composition (combining morphisms)
- Identity (doing nothing)

When your computation can be expressed as morphisms that compose according to categorical laws, you gain:
- **Equational reasoning**: Laws let you transform programs while preserving meaning
- **Fusion**: Intermediate structures can be eliminated
- **Modularity**: Components combine predictably

When your computation cannot be so expressed, you're doing **ordinary computation** - pattern matching, conditionals, arithmetic. This is fine! The goal is to use categorical structure where it helps, not everywhere.

## Recursion Schemes: A Concrete Example

### Inside the Category: Functors and Fixed Points

A **functor** `F` captures the "shape" of one layer of a recursive structure:

```haskell
data SchemaF a
  = PluginF PluginSchema Path [a]   -- 'a' marks recursive positions
  | MethodF MethodSchema Text Path
  deriving Functor
```

The type parameter `a` represents "what goes in the recursive positions." The `Functor` instance tells us how to transform those positions uniformly.

The **fixed point** ties the recursive knot:

```haskell
newtype Fix f = Fix { unFix :: f (Fix f) }

type SchemaTree = Fix SchemaF
-- Unrolling: Fix SchemaF ≅ SchemaF (Fix SchemaF) ≅ SchemaF (SchemaF (Fix SchemaF)) ≅ ...
```

### The Categorical Operations

An **F-algebra** is a morphism `F A → A`. It says: "given one layer with results in the recursive positions, produce a result."

```haskell
methodAlgebra :: SchemaF [MethodInfo] -> [MethodInfo]
methodAlgebra (PluginF schema path childResults) =
  localMethods ++ concat childResults   -- combine children with local data
methodAlgebra (MethodF method ns path) =
  [MethodInfo method path ns]           -- base case
```

An **F-coalgebra** is a morphism `A → F A`. It says: "given a seed, produce one layer with seeds in the recursive positions."

```haskell
schemaCoalgebra :: Path -> SynapseM (SchemaF Path)
schemaCoalgebra path = do
  schema <- fetchSchemaAt path
  pure $ PluginF schema path [path ++ [csNamespace c] | c <- children]
```

### Why This Structure Matters

**Catamorphism** (fold) applies an algebra recursively:

```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

This factors recursion through `fmap`. The algebra only sees one layer at a time - the recursion pattern is abstracted away.

**Anamorphism** (unfold) applies a coalgebra recursively:

```haskell
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
```

**Hylomorphism** composes both - and here's the magic:

```haskell
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
```

Notice: no `Fix` appears in the body! The intermediate tree is **never constructed**. This is **fusion** - a direct consequence of categorical structure.

### The Categorical Laws

These operations satisfy laws that enable equational reasoning:

```
cata alg . ana coalg = hylo alg coalg     -- fusion law
cata id = id                               -- identity
cata alg . cata alg' = cata (alg . alg')  -- composition (when types align)
```

You can transform programs using these laws, confident the meaning is preserved.

## Exiting the Category

At some point, you have a concrete value and need to compute with it. This is where categorical structure ends.

### Example: JSON Schema Processing

Our `schemaToMustache` function processes JSON Schema:

```haskell
schemaToMustache :: Value -> Text
schemaToMustache (Object o) = case KM.lookup "oneOf" o of
  Just (Array variants) -> generateVariants (V.toList variants)
  Nothing -> case KM.lookup "type" o of
    Just (String "object") -> generateObject o
    Just (String "array")  -> generateArray o
    Just (String "string") -> "{{.}}"
    ...
```

This is **not** a catamorphism because:

1. `Value` (from Aeson) is not defined as `Fix JsonSchemaF`
2. We pattern match on concrete constructors, not a functor layer
3. The recursion is explicit, not factored through `fmap`

### Why It's Not Categorical

To make this categorical, we'd need:

```haskell
-- Base functor for JSON Schema
data JsonSchemaF a
  = ObjectF (Map Text a)   -- properties are recursive
  | ArrayF a               -- items schema is recursive
  | OneOfF [a]             -- variants are recursive
  | RefF Text              -- $ref is a leaf (or needs resolution)
  | PrimitiveF PrimType    -- leaf
  deriving Functor

type JsonSchema = Fix JsonSchemaF

-- Now this would be a real catamorphism
schemaToMustache :: JsonSchema -> Text
schemaToMustache = cata alg
  where
    alg :: JsonSchemaF Text -> Text
    alg (ObjectF props) = T.unwords [... | (k, v) <- Map.toList props]
    alg (ArrayF items)  = "{{#.}}" <> items <> "{{/.}}"
    alg (OneOfF vs)     = T.unwords vs
    alg (PrimitiveF _)  = "{{.}}"
```

But Aeson's `Value` type is:

```haskell
data Value
  = Object (KeyMap Value)  -- recursion is built-in, not parameterized
  | Array (Vector Value)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
```

The recursion is **baked in**, not abstracted over a type parameter. We can't `fmap` over it because there's no type parameter to map.

### The Boundary

The boundary between categorical and ordinary computation occurs when:

1. **Types aren't parameterized**: Recursive types without a type parameter can't be functors
2. **External data formats**: JSON, YAML, etc. come as concrete values
3. **Primitive operations**: Arithmetic, string manipulation, comparisons
4. **Effects with complex control flow**: Not all monadic code is categorical
5. **Performance constraints**: Sometimes the abstraction has unacceptable overhead

## Design Principles

### Maximize Categorical Structure

Structure your core domain types as fixed points of functors when:

- You'll traverse/transform them repeatedly
- You want fusion to eliminate intermediate structures
- You want to separate "what to do at each node" from "how to recurse"

```haskell
-- Good: parameterized recursion
data ExprF a
  = LitF Int
  | AddF a a
  | MulF a a
  deriving Functor

type Expr = Fix ExprF

eval :: Expr -> Int
eval = cata alg
  where
    alg (LitF n)   = n
    alg (AddF a b) = a + b
    alg (MulF a b) = a * b
```

### Accept Boundaries Gracefully

When you receive external data, convert to categorical structure at the boundary:

```haskell
-- Parse JSON into categorical structure
parseSchema :: Value -> Maybe JsonSchema
parseSchema = ...  -- ordinary recursion here

-- Now work categorically
processSchema :: JsonSchema -> Result
processSchema = cata myAlgebra
```

Or accept that some processing will be non-categorical:

```haskell
-- This is fine - not everything needs to be an algebra
schemaToMustache :: Value -> Text
schemaToMustache = ...  -- ordinary pattern matching
```

### Recognize the Tradeoffs

| Aspect | Categorical | Ordinary |
|--------|-------------|----------|
| Fusion | Automatic via laws | Manual optimization |
| Reasoning | Equational, lawful | Case-by-case |
| Modularity | Algebras compose | Functions compose |
| Boilerplate | Functor instances, Fix | None |
| Learning curve | Steep | Familiar |
| Flexibility | Constrained by laws | Unconstrained |

## Summary

1. **Categorical structure** (functors, algebras, fixed points) gives you fusion, equational reasoning, and modular composition.

2. **The structure applies** when your types are parameterized over their recursive positions and you can define lawful `Functor` instances.

3. **You exit the structure** when working with concrete values, external data formats, or operations that don't fit the pattern.

4. **The boundary** is where you convert between structured and unstructured representations.

5. **Design principle**: Use categorical structure for your core domain transformations; accept ordinary computation at the edges where external data enters or final results exit.

The goal isn't categorical purity - it's using the right abstraction level for each part of your system. Categorical structure is a tool that provides specific benefits. Use it where those benefits apply; don't force it where it doesn't fit.

## In Synapse

```
                    CATEGORICAL                          ORDINARY

  Path ──coalgebra──▶ SchemaF Path
                          │
                       traverse
                          │
                     SchemaF [MethodInfo]
                          │
                       algebra
                          ▼
                    [MethodInfo] ──────────────────▶ GeneratedTemplate
                                                            │
                                                     methodReturns
                                                            │
                                                            ▼
                                                    Value (JSON Schema)
                                                            │
                                                    schemaToMustache
                                                            │
                                                            ▼
                                                      Text (mustache)
```

The schema tree walking is categorical (hyloM fuses). The JSON Schema to mustache conversion is ordinary pattern matching on Aeson's concrete `Value` type.
