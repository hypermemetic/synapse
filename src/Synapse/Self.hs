{-# LANGUAGE OverloadedStrings #-}

-- | Public surface for the per-backend defaults store.
--
-- Defaults live at @~\/.plexus\/\<backend\>\/defaults.json@ (see 'defaultsPath').
-- The file is a manifest of credential *references* — URIs like
-- @keychain:\/\/svc\/account@ or @literal:raw-value@ — that a resolver
-- registry ('Synapse.Self.Resolve') dereferences at dispatch time.
--
-- This ticket (SELF-1) establishes the types, file format, and resolver
-- API. Actual IO wiring lives in SELF-2 (read), SELF-4 (CLI), SELF-5
-- (write), SELF-7 (core resolvers), SELF-8 (keychain).
module Synapse.Self
  ( -- * File path
    defaultsPath

    -- * On-disk types and encoding
  , StoredDefaults(..)
  , CredentialRef(..)
  , ScopedDefaults(..)
  , emptyStoredDefaults
  , currentVersion
  , encodeDefaults
  , decodeDefaults

    -- * Resolver API
  , ParsedUri(..)
  , ParsedUriBody(..)
  , parseUri
  , Resolver(..)
  , ResolveFn
  , NamedResolver(..)
  , ResolverRegistry(..)
  , emptyRegistry
  , registerResolver
  , registerNamedResolver
  , lookupResolver
  , resolveRef
  , ResolveError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Synapse.Self.Resolve
import Synapse.Self.Types

-- | Path to a backend\'s defaults file: @~\/.plexus\/\<backend\>\/defaults.json@.
--
-- The returned path is relative to @~@ only in the sense that it begins
-- with the literal string @~\/@ — callers wanting the absolute path should
-- expand via 'System.Directory.getHomeDirectory'. This split lets pure
-- code compute the path without IO; the disk IO wiring (SELF-2 / SELF-5)
-- performs the expansion.
defaultsPath :: Text -> FilePath
defaultsPath backend =
  "~" </> ".plexus" </> T.unpack backend </> "defaults.json"
