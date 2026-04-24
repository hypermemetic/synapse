{-# LANGUAGE OverloadedStrings #-}

-- | The @literal:\<value\>@ resolver — the opaque escape hatch.
--
-- Everything after @literal:@ is returned verbatim. No URL-decoding, no
-- trimming: @literal: foo @ yields @" foo "@ (leading\/trailing spaces
-- preserved). @literal:@ (empty body) yields @""@.
--
-- This is the dumbest possible resolver on purpose: the common case is
-- "paste a JWT, don\'t make me think about encoding."
module Synapse.Self.Resolve.Literal
  ( literalResolver
  ) where

import qualified Data.Text as T

import Synapse.Self.Resolve
  ( ParsedUri(..)
  , ParsedUriBody(..)
  , ResolveError(..)
  , ResolveFn
  )
import Synapse.Self.Types (CredentialRef(..))

-- | Resolver for @literal:\<value\>@.
--
-- Only dispatches on 'OpaqueBody' — a hierarchical @literal:\/\/x@ shape
-- is a shape error, reported as 'ResolveBackendError' (the scheme is
-- registered; the caller just used the wrong form).
literalResolver :: ResolveFn
literalResolver ParsedUri{puScheme, puBody} =
  case puBody of
    OpaqueBody body -> pure (Right body)
    HierarchicalBody{} ->
      pure $ Left $ ResolveBackendError
        (CredentialRef (puScheme <> ":<hierarchical>"))
        (T.concat
          [ "literal: scheme must be opaque (literal:<value>), "
          , "not hierarchical (literal://...)"
          ])
