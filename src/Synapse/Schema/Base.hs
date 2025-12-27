{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Base functor for the schema coalgebra
--
-- = The Functor
--
-- @
-- F : Set → Set
-- F(X) = Namespace × Version × Description × Hash × [Method] × Maybe [X]
-- @
--
-- This module provides:
-- - 'PluginSchemaF' — the base functor with a hole for recursion
-- - 'ShallowSchema' — F(ChildSummary), what we receive on the wire
-- - Recursion-schemes instances for folding/unfolding
--
-- = Fixed Points
--
-- - μF (initial algebra) — finite trees, what we build client-side
-- - νF (final coalgebra) — possibly infinite/cyclic, the live plugin network
--
-- The wire format is F(ChildSummary). We unfold lazily via network calls.
module Synapse.Schema.Base
  ( -- * The Base Functor
    PluginSchemaF(..)
  , ShallowSchema

    -- * Conversion
  , toShallow
  , fromShallow

    -- * Re-exports for convenience
  , Fix(..)
  , cata
  , para
  , ana
  , hylo
  ) where

import Data.Functor.Foldable (cata, para, ana, hylo)
import Data.Fix (Fix(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Synapse.Schema.Types

-- | One layer of the schema with a hole for recursion
--
-- This is the base functor F. The parameter 'a' represents:
-- - 'ChildSummary' for shallow/wire format
-- - 'PluginSchemaF a' for recursive expansion
-- - Any carrier type for algebras
data PluginSchemaF a = PluginSchemaF
  { psfNamespace   :: !Text
  , psfVersion     :: !Text
  , psfDescription :: !Text
  , psfHash        :: !PluginHash
  , psfMethods     :: ![MethodSchema]
  , psfChildren    :: !(Maybe [a])
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | The wire format: one layer with child references
type ShallowSchema = PluginSchemaF ChildSummary

-- | Convert PluginSchema to the base functor applied to ChildSummary
toShallow :: PluginSchema -> ShallowSchema
toShallow PluginSchema{..} = PluginSchemaF
  { psfNamespace   = psNamespace
  , psfVersion     = psVersion
  , psfDescription = psDescription
  , psfHash        = psHash
  , psfMethods     = psMethods
  , psfChildren    = psChildren
  }

-- | Convert back from the base functor
fromShallow :: ShallowSchema -> PluginSchema
fromShallow PluginSchemaF{..} = PluginSchema
  { psNamespace   = psfNamespace
  , psVersion     = psfVersion
  , psDescription = psfDescription
  , psHash        = psfHash
  , psMethods     = psfMethods
  , psChildren    = psfChildren
  }
