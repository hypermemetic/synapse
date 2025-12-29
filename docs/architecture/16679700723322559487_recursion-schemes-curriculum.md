# Recursion Schemes: A 6-Month Curriculum

**For:** Rust/Go developers with no Haskell or category theory background

**Goal:** Understand recursion schemes well enough to read, modify, and design algebraic APIs

---

## Month 1: Recursive Data Structures

**Starting point:** You know how to write recursive functions over trees.

### Week 1-2: Trees in Rust/Go

Write these from scratch without libraries:

```rust
enum Tree<T> {
    Leaf(T),
    Node(Box<Tree<T>>, Box<Tree<T>>),
}

// Implement:
fn sum(tree: &Tree<i32>) -> i32
fn map<A, B>(tree: Tree<A>, f: impl Fn(A) -> B) -> Tree<B>
fn fold<A, B>(tree: Tree<A>, leaf: impl Fn(A) -> B, node: impl Fn(B, B) -> B) -> B
```

**Key insight:** `fold` takes two functions - one for leaves, one for combining. The recursion pattern is always the same.

### Week 3-4: Multiple Tree Operations

Implement 5+ different operations on the same tree:
- sum, product
- depth, size
- contains, find
- to_list, to_string

Notice: every one has the same recursive structure. Only the "what to do at each node" changes.

**Milestone:** You're annoyed by the repetition.

---

## Month 2: Factoring Out Recursion

**Goal:** Separate "tree shape" from "what's in the tree"

### Week 1-2: The Functor Pattern

```rust
// Before: recursion baked in
enum Tree<T> {
    Leaf(T),
    Node(Box<Tree<T>>, Box<Tree<T>>),
}

// After: recursion is a parameter
enum TreeF<T, Recurse> {
    Leaf(T),
    Node(Recurse, Recurse),
}

// TreeF<T, X> means "one layer of tree, with X in child positions"
// TreeF<i32, ()>        - one layer, no children
// TreeF<i32, i32>       - one layer, children already reduced to i32
// TreeF<i32, Tree<i32>> - one layer, children are full subtrees
```

### Week 3-4: Implement map for TreeF

```rust
impl<T, A> TreeF<T, A> {
    fn map_children<B>(self, f: impl Fn(A) -> B) -> TreeF<T, B> {
        match self {
            TreeF::Leaf(x) => TreeF::Leaf(x),
            TreeF::Node(l, r) => TreeF::Node(f(l), f(r)),
        }
    }
}
```

This is `fmap`. It transforms what's in the child positions without knowing what type it is.

**Milestone:** You understand that `TreeF` is a "template" for one layer, and the type parameter says what goes in the holes.

---

## Month 3: Fixed Points and Folds

**Goal:** Reconstruct full trees from layers, then tear them down

### Week 1-2: The Fixed Point

```rust
// "A tree is a layer that contains trees"
struct Fix<F> {
    unfix: F<Fix<F>>  // pseudo-Rust, needs GATs or boxed traits
}

// Expanding:
// Fix<TreeF<i32>>
//   = TreeF<i32, Fix<TreeF<i32>>>
//   = TreeF<i32, TreeF<i32, Fix<TreeF<i32>>>>
//   = ... (infinite nesting, but lazy)
```

In practice (Rust):
```rust
enum Tree<T> {
    Leaf(T),
    Node(Box<Tree<T>>, Box<Tree<T>>),
}

// This IS Fix<TreeF<T>> - the recursion is "tied"
```

### Week 3-4: Catamorphism (Fold)

```rust
// An "algebra" says how to reduce one layer
type Algebra<F, A> = impl Fn(F<A>) -> A;

// Catamorphism applies it recursively
fn cata<F, A>(alg: Algebra<F, A>, tree: Fix<F>) -> A {
    let layer = tree.unfix;                    // peel one layer
    let mapped = layer.map_children(|child| cata(alg, child));  // recurse
    alg(mapped)                                // apply algebra
}
```

Example:
```rust
fn sum_algebra(layer: TreeF<i32, i32>) -> i32 {
    match layer {
        TreeF::Leaf(n) => n,
        TreeF::Node(l, r) => l + r,  // l and r are already i32!
    }
}

let total = cata(sum_algebra, my_tree);
```

**Milestone:** You see that the algebra only handles ONE layer. The recursion is handled by `cata`.

---

## Month 4: Unfolds and Builds

**Goal:** Build trees from seeds

### Week 1-2: Coalgebra and Anamorphism

```rust
// A "coalgebra" says how to produce one layer from a seed
type Coalgebra<F, A> = impl Fn(A) -> F<A>;

// Anamorphism applies it recursively to build a tree
fn ana<F, A>(coalg: Coalgebra<F, A>, seed: A) -> Fix<F> {
    let layer = coalg(seed);                   // produce one layer
    let mapped = layer.map_children(|s| ana(coalg, s));  // recurse on child seeds
    Fix { unfix: mapped }                      // wrap it up
}
```

Example - build a tree from a range:
```rust
fn range_coalgebra(n: i32) -> TreeF<i32, i32> {
    if n <= 1 {
        TreeF::Leaf(n)
    } else {
        TreeF::Node(n - 1, n - 2)  // child seeds
    }
}

let tree = ana(range_coalgebra, 5);
```

### Week 3-4: Real-World Unfolds

Your schema walker is an unfold:
```rust
fn schema_coalgebra(path: Vec<String>) -> SchemaF<Vec<String>> {
    let schema = fetch_schema(&path);
    let child_paths: Vec<Vec<String>> = schema.children
        .iter()
        .map(|c| [&path[..], &[c.namespace.clone()]].concat())
        .collect();
    SchemaF::Plugin { schema, path, children: child_paths }
}
```

**Milestone:** Coalgebra = "given a seed, what's the next layer and what are the child seeds?"

---

## Month 5: Hylomorphism and Fusion

**Goal:** Combine unfold + fold efficiently

### Week 1-2: Hylomorphism

```rust
// Unfold then fold:
fn hylo<F, A, B>(alg: Algebra<F, B>, coalg: Coalgebra<F, A>, seed: A) -> B {
    cata(alg, ana(coalg, seed))  // build tree, then tear it down
}

// But this builds an intermediate tree! Rewrite fused:
fn hylo<F, A, B>(alg: Algebra<F, B>, coalg: Coalgebra<F, A>, seed: A) -> B {
    let layer = coalg(seed);                        // produce one layer
    let mapped = layer.map_children(|s| hylo(alg, coalg, s));  // recurse
    alg(mapped)                                     // consume one layer
}
```

No `Fix` in the fused version! The intermediate tree is never built.

### Week 3-4: Why Fusion Matters

```rust
// Without fusion: O(n) allocations for tree, then O(n) traversal
let tree = ana(build_coalg, seed);  // allocate entire tree
let result = cata(sum_alg, tree);   // traverse entire tree

// With fusion: O(1) extra allocation, single pass
let result = hylo(sum_alg, build_coalg, seed);  // never builds tree
```

This is why the categorical formulation matters - the laws guarantee this optimization is valid.

**Milestone:** `hylo` = unfold and fold in one pass, no intermediate structure.

---

## Month 6: Effects and Real Systems

**Goal:** Handle IO, errors, async in the same framework

### Week 1-2: Monadic Variants

When unfolding requires IO (like fetching schemas):

```rust
// Effectful coalgebra
async fn schema_coalgebra(path: Vec<String>) -> Result<SchemaF<Vec<String>>> {
    let schema = fetch_schema(&path).await?;
    // ...
}

// Effectful anamorphism
async fn ana_m<F, A>(coalg: impl Fn(A) -> Result<F<A>>, seed: A) -> Result<Fix<F>> {
    let layer = coalg(seed)?;
    let mapped = layer.try_map_children(|s| ana_m(coalg, s)).await?;
    Ok(Fix { unfix: mapped })
}
```

### Week 3-4: Putting It Together

Your actual system:

```
Path (seed)
    │
    ▼ coalgebra (fetch schema, produce child paths)
SchemaF<Path>
    │
    ▼ traverse (apply recursion to each child)
SchemaF<Vec<MethodInfo>>
    │
    ▼ algebra (combine local methods with child results)
Vec<MethodInfo>
```

All in one fused pass via `hyloM`.

**Milestone:** You can read and modify the Synapse recursion scheme code.

---

## Summary: The Pattern

```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│   SEED ──coalgebra──▶ F<SEED> ──map recurse──▶ F<RESULT>   │
│                                       │                     │
│                                       ▼                     │
│                                   algebra                   │
│                                       │                     │
│                                       ▼                     │
│                                    RESULT                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘

coalgebra: "How do I get one layer from a seed?"
algebra:   "How do I combine one layer of results?"
F:         "What's the shape of one layer?"
map:       "Apply a function to child positions"
```

---

## Vocabulary Cheat Sheet

| Term | Plain English | Rust Analogy |
|------|---------------|--------------|
| Functor | Container you can map over | `Iterator::map`, `Option::map` |
| Algebra | "One layer with results → result" | Reducer function |
| Coalgebra | "Seed → one layer with seeds" | Iterator's `next()` |
| Catamorphism | Fold using algebra | `fold` / `reduce` |
| Anamorphism | Unfold using coalgebra | `unfold` / `from_fn` |
| Hylomorphism | Unfold then fold (fused) | `iter().map().fold()` |
| Fixed point | Recursive type from functor | The recursive enum itself |

---

## Exercises for Each Phase

*Details to be added upon request for each month.*

### Month 1 Exercises
<!-- TODO: Tree implementation exercises -->

### Month 2 Exercises
<!-- TODO: Functor extraction exercises -->

### Month 3 Exercises
<!-- TODO: Catamorphism exercises -->

### Month 4 Exercises
<!-- TODO: Anamorphism exercises -->

### Month 5 Exercises
<!-- TODO: Hylomorphism exercises -->

### Month 6 Exercises
<!-- TODO: Effectful recursion scheme exercises -->

---

## Further Reading

*To be added upon request:*
- Academic papers (approachable ones)
- Blog posts by difficulty
- Video lectures
- Code examples in Rust/Go
