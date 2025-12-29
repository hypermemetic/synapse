-- | Schema tree walking via recursion schemes
--
-- This module provides schema traversal using proper categorical machinery:
--
-- = Anamorphism (Unfold)
--
-- The schema tree is built via anamorphism - we have a coalgebra that
-- produces one layer of 'SchemaF' from a path seed:
--
-- @
-- coalgebra :: Path -> SynapseM (SchemaF Path)
-- @
--
-- Iterating this coalgebra builds the full tree:
--
-- @
-- unfoldSchema :: Path -> SynapseM SchemaTree
-- unfoldSchema = anaM coalgebra
-- @
--
-- = Catamorphism (Fold)
--
-- To process the tree, we use algebras. A method-collecting algebra:
--
-- @
-- methodAlgebra :: SchemaF [MethodInfo] -> [MethodInfo]
-- methodAlgebra (PluginF schema path children) =
--   localMethods ++ concat children
-- methodAlgebra (MethodF method ns path) =
--   [MethodInfo method path ns]
-- @
--
-- = Hylomorphism (Unfold then Fold)
--
-- Most operations combine unfold and fold. Using 'hyloM' we get fusion:
--
-- @
-- walkMethods :: Path -> SynapseM [MethodInfo]
-- walkMethods = hyloM methodAlgebra schemaCoalgebra
-- @
--
-- The fused version never materializes the intermediate tree!
module Synapse.Algebra.Walk
  ( -- * Recursion Schemes
    walkMethods
  , walkSchema
  , foldMethods

    -- * Building Trees
  , unfoldSchema
  , foldSchema

    -- * Types
  , MethodInfo(..)
  , SchemaF(..)
  , SchemaTree

    -- * Re-exports for algebras
  , Fix(..)
  ) where

import Data.Text (Text)

import Synapse.Schema.Types (Path, PluginSchema(..), MethodSchema(..), ChildSummary(..))
import Synapse.Schema.Functor (SchemaF(..), Fix(..), SchemaTree)
import Synapse.Monad
import Synapse.Algebra.Recursion (hyloM, unfoldSchema, foldSchema)
import Synapse.Transport (fetchSchemaAt)

-- | Information about a method in context
data MethodInfo = MethodInfo
  { miMethod    :: MethodSchema   -- ^ The method schema
  , miPath      :: Path           -- ^ Full path to this method
  , miNamespace :: Text           -- ^ Parent namespace
  }
  deriving stock (Show, Eq)

-- ============================================================================
-- Algebras
-- ============================================================================

-- | Algebra for collecting methods
--
-- This is a proper F-algebra: SchemaF [MethodInfo] -> [MethodInfo]
-- It shows how to combine child results with local data.
methodAlgebra :: SchemaF [MethodInfo] -> [MethodInfo]
methodAlgebra (PluginF schema path childResults) =
  -- Local methods from this plugin
  let localMethods =
        [ MethodInfo m (path ++ [methodName m]) (psNamespace schema)
        | m <- psMethods schema
        ]
  -- Combine with results from children (already processed)
  in localMethods ++ concat childResults
methodAlgebra (MethodF method ns path) =
  -- Method nodes are leaves - just wrap in list
  [MethodInfo method path ns]

-- | Monadic algebra for collecting methods (for hyloM)
methodAlgebraM :: SchemaF [MethodInfo] -> SynapseM [MethodInfo]
methodAlgebraM = pure . methodAlgebra

-- ============================================================================
-- Coalgebras
-- ============================================================================

-- | Coalgebra for unfolding schema trees
--
-- This is a proper F-coalgebra: Path -> SynapseM (SchemaF Path)
-- It shows how to produce one layer from a seed.
schemaCoalgebra :: Path -> SynapseM (SchemaF Path)
schemaCoalgebra path = do
  schema <- fetchSchemaAt path
  let childPaths = case psChildren schema of
        Nothing -> []
        Just children -> [path ++ [csNamespace c] | c <- children]
  pure $ PluginF schema path childPaths

-- ============================================================================
-- Walking Operations
-- ============================================================================

-- | Walk the schema tree, collecting all methods
--
-- This is a hylomorphism: unfold from path, fold with method algebra.
-- Uses hyloM for the fused monadic version - no intermediate tree built!
--
-- @
-- walkMethods = hyloM methodAlgebraM schemaCoalgebra
-- @
walkMethods :: Path -> SynapseM [MethodInfo]
walkMethods = hyloM methodAlgebraM schemaCoalgebra

-- | Walk schema tree with a custom monadic algebra
--
-- General version - provide your own algebra.
walkSchema :: (SchemaF a -> SynapseM a) -> Path -> SynapseM a
walkSchema alg = hyloM alg schemaCoalgebra

-- | Fold over methods with pure transformations
--
-- Convenience wrapper: walk to collect methods, then map and combine.
foldMethods :: (MethodInfo -> a)  -- ^ Transform each method
            -> ([a] -> b)          -- ^ Combine results
            -> Path                -- ^ Starting path
            -> SynapseM b
foldMethods f combine path = do
  methods <- walkMethods path
  pure $ combine $ map f methods
