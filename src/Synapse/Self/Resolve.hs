{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | URI parsing and resolver registry for credential references.
--
-- A 'CredentialRef' on disk is one of:
--
-- * Opaque: @literal:\<rawValue\>@ — value taken verbatim after the first
--   colon. No URL-encoding so JWTs / base64 fit cleanly.
-- * Hierarchical: @scheme:\/\/authority\/path[?query]@ — interpretation
--   is scheme-specific.
--
-- Parsing is shape-only: 'parseUri' accepts any well-formed URI. Scheme
-- validity is enforced at *resolve* time via the registry, so the file
-- parses even when a scheme\'s backend (e.g. @vault@) is not wired up.
--
-- The registry is an open map; concrete resolvers (e.g. @keychain@,
-- @env@, @file@) are registered by their owning packages via simple
-- 'Map' insertion. An empty registry (@mempty@) returns
-- 'ResolveUnknownScheme' for every lookup.
module Synapse.Self.Resolve
  ( -- * Parsed URI shape
    ParsedUri(..)
  , ParsedUriBody(..)
  , parseUri

    -- * Resolver interface
  , Resolver(..)
  , ResolveFn
  , NamedResolver(..)

    -- * Registry
  , ResolverRegistry(..)
  , emptyRegistry
  , registerResolver
  , registerNamedResolver
  , lookupResolver
  , resolveRef

    -- * Errors
  , ResolveError(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T

import Synapse.Self.Types (CredentialRef(..))

-- ============================================================================
-- Parsed URI
-- ============================================================================

-- | Body of a parsed URI.
--
-- We keep this deliberately simple — this is not RFC 3986. We only need
-- to separate the opaque escape hatch from the @scheme://authority/path@
-- shape, plus a light query split so @env:\/\/VAR?fallback=x@ style
-- extensions are possible later without another parser pass.
data ParsedUriBody
  = OpaqueBody Text
    -- ^ Everything after the first @:@ for non-hierarchical URIs.
  | HierarchicalBody
      { puAuthority :: Text
      , puPath      :: Text
      , puQuery     :: [(Text, Text)]
      }
  deriving stock (Show, Eq)

-- | A parsed credential URI.
data ParsedUri = ParsedUri
  { puScheme :: Text
  , puBody   :: ParsedUriBody
  }
  deriving stock (Show, Eq)

-- | Parse a 'CredentialRef' URI.
--
-- Splits on the first @:@ to extract the scheme. If the remainder starts
-- with @\/\/@ we treat it as hierarchical (authority + path + optional
-- query); otherwise the remainder is taken verbatim as the opaque body.
--
-- Rejected shapes:
--
-- * bare strings with no scheme (e.g. @"abc"@)
-- * empty scheme (e.g. @":foo"@)
parseUri :: CredentialRef -> Either Text ParsedUri
parseUri (CredentialRef raw) =
  case T.breakOn ":" raw of
    (_, "") -> Left ("missing scheme in credential reference: " <> raw)
    (scheme, colonRest)
      | T.null scheme -> Left ("empty scheme in credential reference: " <> raw)
      | otherwise ->
          let rest = T.drop 1 colonRest
          in case T.stripPrefix "//" rest of
               Just hier -> Right (ParsedUri scheme (parseHierarchical hier))
               Nothing   -> Right (ParsedUri scheme (OpaqueBody rest))

-- | Split @authority/path?query@. Path and authority are empty when absent.
parseHierarchical :: Text -> ParsedUriBody
parseHierarchical s =
  let (beforeQ, afterQ) = T.breakOn "?" s
      query = if T.null afterQ then [] else parseQuery (T.drop 1 afterQ)
      (authority, path) = case T.breakOn "/" beforeQ of
        (a, "")    -> (a, "")
        (a, slash) -> (a, slash)
  in HierarchicalBody
       { puAuthority = authority
       , puPath      = path
       , puQuery     = query
       }

-- | Parse @k1=v1&k2=v2@. No URL-decoding — keys and values are taken
-- verbatim. Missing @=@ yields an empty value.
parseQuery :: Text -> [(Text, Text)]
parseQuery s = map splitPair (T.splitOn "&" s)
  where
    splitPair pair = case T.breakOn "=" pair of
      (k, "")     -> (k, "")
      (k, eqRest) -> (k, T.drop 1 eqRest)

-- ============================================================================
-- Resolver interface
-- ============================================================================

-- | The shape resolvers implement. @IO@ because most backends
-- (keychain, filesystem, vault) are inherently effectful.
type ResolveFn = ParsedUri -> IO (Either ResolveError Text)

-- | A 'Resolver' knows its own scheme and how to dereference a URI.
--
-- Kept as a typeclass for ergonomic instance declarations, but the
-- registry stores the bare 'ResolveFn' so resolvers defined outside
-- this package (e.g. a future @plexus-vault@) compose without any
-- typeclass gymnastics.
class Resolver r where
  scheme  :: Proxy r -> Text
  resolve :: r -> ResolveFn

-- | A named resolver for debugging / diagnostics — pair the scheme
-- string with its 'ResolveFn' without going through the typeclass.
data NamedResolver = NamedResolver
  { nrScheme  :: Text
  , nrResolve :: ResolveFn
  }

-- ============================================================================
-- Registry
-- ============================================================================

-- | Map from scheme to resolver function. Construction via 'mempty'
-- yields an empty registry that always returns 'ResolveUnknownScheme'.
newtype ResolverRegistry = ResolverRegistry
  { schemes :: Map Text ResolveFn
  }

instance Semigroup ResolverRegistry where
  -- Right-biased union matches 'Map.union'\'s left-bias when read as
  -- @a <> b == a@ wins: we flip it so later registrations override.
  ResolverRegistry a <> ResolverRegistry b = ResolverRegistry (Map.union b a)

instance Monoid ResolverRegistry where
  mempty = ResolverRegistry Map.empty

-- | An empty registry. Equivalent to 'mempty'; provided for discoverability.
emptyRegistry :: ResolverRegistry
emptyRegistry = mempty

-- | Register a resolver function under a scheme. Last write wins.
registerResolver :: Text -> ResolveFn -> ResolverRegistry -> ResolverRegistry
registerResolver s fn (ResolverRegistry m) =
  ResolverRegistry (Map.insert s fn m)

-- | Register a 'NamedResolver'. Convenience for the typeclass-free path.
registerNamedResolver :: NamedResolver -> ResolverRegistry -> ResolverRegistry
registerNamedResolver NamedResolver{..} = registerResolver nrScheme nrResolve

-- | Look up the resolver for a scheme.
lookupResolver :: Text -> ResolverRegistry -> Maybe ResolveFn
lookupResolver s (ResolverRegistry m) = Map.lookup s m

-- | Resolve a 'CredentialRef' end-to-end: parse, dispatch, and report
-- either the resolved value or a structured 'ResolveError'. Parse
-- failures surface as 'ResolveParseError' so callers get a single error
-- channel.
resolveRef :: ResolverRegistry -> CredentialRef -> IO (Either ResolveError Text)
resolveRef reg ref =
  case parseUri ref of
    Left err -> pure (Left (ResolveParseError err))
    Right parsed ->
      case lookupResolver (puScheme parsed) reg of
        Nothing -> pure (Left (ResolveUnknownScheme (puScheme parsed)))
        Just fn -> fn parsed

-- ============================================================================
-- Errors
-- ============================================================================

-- | Operator-facing failure modes for credential reference resolution.
--
-- These are what @_self resolve@ (SELF-4) surfaces — structured so UIs
-- can render @"keychain item 'uscis/access_token' not found"@ instead of
-- @"error 127"@.
data ResolveError
  = ResolveUnknownScheme Text
    -- ^ No resolver registered for this scheme.
  | ResolveNotFound CredentialRef
    -- ^ Scheme resolved but the underlying item is absent
    --   (keychain miss, env var unset, file doesn\'t exist).
  | ResolveBackendError CredentialRef Text
    -- ^ Scheme resolver hit an unexpected error from its backend.
  | ResolveParseError Text
    -- ^ The reference URI itself was malformed.
  deriving stock (Show, Eq)
