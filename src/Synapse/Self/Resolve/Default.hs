{-# LANGUAGE OverloadedStrings #-}

-- | The default 'ResolverRegistry' for the baseline deployment shapes.
--
-- Registers the four resolvers every deployment needs:
--
-- * @literal:\<value\>@ — opaque escape hatch (see 'literalResolver').
-- * @env:\/\/\<VAR\>@  — environment variable (see 'envResolver').
-- * @file:\/\/\<path\>@ — file contents (see 'fileResolver').
-- * @keychain:\/\/\<svc\>\/\<acct\>@ — OS keychain (see 'keychainResolver';
--   full support on macOS, clear error on other platforms).
--
-- Registering @keychain@ unconditionally (including on platforms where
-- it's a stub) lets @_self show@ list @keychain:\/\/@ refs with a
-- specific "not implemented on <platform>" error instead of the less
-- helpful "unknown scheme" error.
--
-- Callers extend via 'registerResolver':
--
-- > let reg = registerResolver "vault" myVaultResolver defaultRegistry
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
import Synapse.Self.Resolve.Keychain (keychainResolver)
import Synapse.Self.Resolve.Literal (literalResolver)

-- | A registry pre-populated with the four baseline resolvers:
-- @literal@, @env@, @file@, @keychain@.
defaultRegistry :: ResolverRegistry
defaultRegistry =
    registerResolver "literal"  literalResolver
  $ registerResolver "env"      envResolver
  $ registerResolver "file"     fileResolver
  $ registerResolver "keychain" keychainResolver
  $ emptyRegistry
