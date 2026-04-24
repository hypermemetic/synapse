{-# LANGUAGE OverloadedStrings #-}

-- | The @env:\/\/\<VAR\>@ resolver — read credentials from environment
-- variables.
--
-- The authority slot carries the variable name; path and query are
-- ignored. @env:\/\/USCIS_JWT@ looks up @USCIS_JWT@ via
-- 'System.Environment.lookupEnv':
--
-- * set → 'Right' value
-- * unset → 'ResolveNotFound'
--
-- Using the authority slot for the variable name is a slight RFC 3986
-- stretch (authority is supposed to be host:port) but it reads right.
-- Alternatives like @env:VAR@ (opaque) or @env:\/\/\/VAR@ (path) were
-- uglier for no gain.
module Synapse.Self.Resolve.Env
  ( envResolver
  ) where

import qualified Data.Text as T
import System.Environment (lookupEnv)

import Synapse.Self.Resolve
  ( ParsedUri(..)
  , ParsedUriBody(..)
  , ResolveError(..)
  , ResolveFn
  )
import Synapse.Self.Types (CredentialRef(..))

-- | Resolver for @env:\/\/\<VAR\>@.
--
-- Dispatches only on 'HierarchicalBody'. An opaque @env:VAR@ form is a
-- shape error surfaced as 'ResolveBackendError'.
envResolver :: ResolveFn
envResolver ParsedUri{puScheme, puBody} =
  case puBody of
    HierarchicalBody{puAuthority = varName}
      | T.null varName ->
          pure $ Left $ ResolveBackendError
            (CredentialRef (puScheme <> "://"))
            "env: missing variable name (expected env://VAR)"
      | otherwise -> do
          mVal <- lookupEnv (T.unpack varName)
          case mVal of
            Just v  -> pure (Right (T.pack v))
            Nothing -> pure $ Left $
              ResolveNotFound (CredentialRef (puScheme <> "://" <> varName))
    OpaqueBody body ->
      pure $ Left $ ResolveBackendError
        (CredentialRef (puScheme <> ":" <> body))
        "env: scheme must be hierarchical (env://VAR), not opaque (env:VAR)"
