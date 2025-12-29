-- | Schema tree walking algebra
--
-- Provides an anamorphism for unfolding the schema tree and
-- a catamorphism for folding over methods.
--
-- = The Algebra
--
-- @
-- walk :: (PluginSchema -> [a] -> b) -> (MethodSchema -> Path -> a) -> Path -> SynapseM b
-- @
--
-- The walk is coalgebraic - we lazily unfold the tree via schema fetches,
-- then fold over methods with the provided algebra.
module Synapse.Algebra.Walk
  ( -- * Walking
    walkSchema
  , walkMethods
  , foldMethods

    -- * Types
  , MethodInfo(..)
  ) where

import Control.Monad (forM)
import Data.Text (Text)

import Synapse.Schema.Types
import Synapse.Monad
import Synapse.Transport (fetchSchemaAt)

-- | Information about a method in context
data MethodInfo = MethodInfo
  { miMethod    :: MethodSchema   -- ^ The method schema
  , miPath      :: Path           -- ^ Full path to this method (namespace.method)
  , miNamespace :: Text           -- ^ Parent namespace
  }
  deriving stock (Show, Eq)

-- | Walk the schema tree, collecting all methods
-- This is an anamorphism - we unfold the tree structure
walkMethods :: Path -> SynapseM [MethodInfo]
walkMethods path = do
  schema <- fetchSchemaAt path
  let localMethods =
        [ MethodInfo m (path ++ [methodName m]) (psNamespace schema)
        | m <- psMethods schema
        ]
  childMethods <- case psChildren schema of
    Nothing -> pure []
    Just children -> do
      nested <- forM children $ \child ->
        walkMethods (path ++ [csNamespace child])
      pure $ concat nested
  pure $ localMethods ++ childMethods

-- | Walk schema tree applying an algebra to each plugin
-- Returns the transformed tree structure
walkSchema :: (PluginSchema -> Path -> [a] -> SynapseM a)  -- ^ Plugin algebra
           -> Path                                          -- ^ Starting path
           -> SynapseM a
walkSchema algebra path = do
  schema <- fetchSchemaAt path
  childResults <- case psChildren schema of
    Nothing -> pure []
    Just children -> forM children $ \child ->
      walkSchema algebra (path ++ [csNamespace child])
  algebra schema path childResults

-- | Fold over all methods in the tree with a pure function
-- This is a catamorphism over the method collection
foldMethods :: (MethodInfo -> a)  -- ^ Transform each method
            -> ([a] -> b)          -- ^ Combine results
            -> Path                -- ^ Starting path
            -> SynapseM b
foldMethods f combine path = do
  methods <- walkMethods path
  pure $ combine $ map f methods
