{-# LANGUAGE OverloadedStrings #-}

-- | The default 'ResolverRegistry' for the baseline deployment shapes.
--
-- Registers the three resolvers every deployment needs:
--
-- * @literal:\<value\>@ — opaque escape hatch (see 'literalResolver').
-- * @env:\/\/\<VAR\>@  — environment variable (see 'envResolver').
-- * @file:\/\/\<path\>@ — file contents (see 'fileResolver').
--
-- Callers extend via 'registerResolver':
--
-- > let reg = registerResolver "vault" myVaultResolver defaultRegistry
--
-- Additional baseline schemes (e.g. @keychain:\/\/@ in SELF-8) land here.
module Synapse.Self.Resolve.Default
  ( defaultRegistry
  ) where

import Synapse.Self.Resolve
  ( ResolverRegistry
  , emptyRegistry
  , registerResolver
  )
import Synapse.Self.Resolve.Env (envResolver)
import Synapse.Self.Resolve.File (fileResolver)
import Synapse.Self.Resolve.Literal (literalResolver)

-- | A registry pre-populated with the three baseline resolvers:
-- @literal@, @env@, @file@.
defaultRegistry :: ResolverRegistry
defaultRegistry =
    registerResolver "literal" literalResolver
  $ registerResolver "env"     envResolver
  $ registerResolver "file"    fileResolver
  $ emptyRegistry
