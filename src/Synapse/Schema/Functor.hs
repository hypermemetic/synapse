{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Base functor for schema trees
--
-- This module defines the "shape" of one layer of a schema tree,
-- enabling proper recursion schemes (ana/cata/hylo).
--
-- = The Functor
--
-- @
-- data SchemaF a
--   = PluginF PluginSchema [a]   -- interior node: plugin with children
--   | MethodF MethodSchema Text  -- leaf: method with namespace
-- @
--
-- The type parameter 'a' represents "what's in the recursive positions".
-- For a tree, 'a' would be the subtrees. For a fold result, 'a' would be
-- the accumulated value from children.
--
-- = Fixed Point
--
-- The fixed point @Fix SchemaF@ gives us the infinite tree type:
--
-- @
-- type SchemaTree = Fix SchemaF
-- @
--
-- This is isomorphic to a tree where every node is either a plugin
-- (with children) or a method (leaf).
module Synapse.Schema.Functor
  ( -- * Base Functor
    SchemaF(..)

    -- * Fixed Point
  , Fix(..)
  , SchemaTree

    -- * Seed for unfolding
  , SchemaSeed(..)

    -- * Accessors
  , nodeNamespace
  , nodeMethods
  ) where

import Data.Text (Text)

import Synapse.Schema.Types (PluginSchema(..), MethodSchema(..), Path)

-- | Fixed point of a functor
--
-- @Fix f@ is the type where @f@ refers to itself in recursive positions.
-- Unrolling: @Fix f ≅ f (Fix f) ≅ f (f (Fix f)) ≅ ...@
newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- | Base functor for schema trees
--
-- Captures the "shape" of one layer:
-- - PluginF: interior node with plugin info and child positions
-- - MethodF: leaf with method info and parent namespace
data SchemaF a
  = PluginF
      { sfSchema    :: PluginSchema  -- ^ The plugin schema at this node
      , sfPath      :: Path          -- ^ Path to this node
      , sfChildren  :: [a]           -- ^ Recursive positions (children)
      }
  | MethodF
      { sfMethod    :: MethodSchema  -- ^ The method schema
      , sfNamespace :: Text          -- ^ Parent namespace
      , sfMethodPath :: Path         -- ^ Full path to this method
      }
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

-- | The fixed point gives us schema trees
type SchemaTree = Fix SchemaF

-- | Seed for anamorphic unfolding
-- Contains the path to fetch and any context needed
data SchemaSeed = SchemaSeed
  { seedPath :: Path
  }
  deriving stock (Show, Eq)

-- | Get the namespace from a schema node
nodeNamespace :: SchemaF a -> Text
nodeNamespace (PluginF schema _ _) = psNamespace schema
nodeNamespace (MethodF _ ns _) = ns

-- | Get methods from a plugin node (empty for method nodes)
nodeMethods :: SchemaF a -> [MethodSchema]
nodeMethods (PluginF schema _ _) = psMethods schema
nodeMethods (MethodF _ _ _) = []
