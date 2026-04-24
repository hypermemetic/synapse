{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IO glue for the per-backend defaults store (SELF-2).
--
-- Three pieces live here:
--
-- * 'loadDefaults' — read @~\/.plexus\/\<backend\>\/defaults.json@ from disk.
-- * 'resolveAll'   — dispatch every 'CredentialRef' in a 'StoredDefaults'
--                    through a 'ResolverRegistry', yielding a
--                    'ResolvedDefaults' with concrete values.
-- * 'merge'        — pure left-biased merge of CLI-provided cookies\/headers
--                    over the resolved stored defaults. CLI wins per key.
--
-- The three compose into the read path that synapse\'s request builder uses
-- on every invocation: load → resolve → merge → hand to the upgrade.
module Synapse.Self.IO
  ( -- * Resolved defaults (concrete values, not refs)
    ResolvedDefaults(..)
  , emptyResolvedDefaults

    -- * Aliases used by the pipeline
  , MethodPath
  , Cookies
  , Headers

    -- * Load / resolve / merge
  , loadDefaults
  , resolveAll
  , merge

    -- * Write path (stub; SELF-5 tightens to atomic + chmod 600)
  , writeDefaults
  , absDefaultsPath
  ) where

import Control.Exception (IOException, throwIO, try)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, userError)

import Synapse.Self.Resolve
  ( ResolveError(..)
  , ResolverRegistry
  , resolveRef
  )
import Synapse.Self.Types
  ( CredentialRef
  , StoredDefaults(..)
  , decodeDefaults
  , encodeDefaults
  , emptyStoredDefaults
  )

-- ============================================================================
-- Types used by the request-building pipeline
-- ============================================================================

-- | A method path. v1 ignores the contents entirely — it exists so SELF-7+
-- can introduce scope matching without a signature change.
type MethodPath = [Text]

-- | Cookies attached to the WebSocket upgrade, keyed by name.
type Cookies = Map Text Text

-- | Extra HTTP headers attached to the upgrade, keyed by name.
type Headers = Map Text Text

-- | A 'StoredDefaults' after every 'CredentialRef' has been dereferenced
-- through a 'ResolverRegistry'. Parallels the on-disk shape but carries
-- concrete 'Text' values instead of refs.
data ResolvedDefaults = ResolvedDefaults
  { rdCookies :: !Cookies
  , rdHeaders :: !Headers
  }
  deriving stock (Show, Eq)

-- | Empty resolved defaults — what 'resolveAll' returns for
-- 'emptyStoredDefaults'.
emptyResolvedDefaults :: ResolvedDefaults
emptyResolvedDefaults = ResolvedDefaults
  { rdCookies = Map.empty
  , rdHeaders = Map.empty
  }

-- ============================================================================
-- loadDefaults
-- ============================================================================

-- | Compute the absolute path to a backend's defaults file, expanding @~@
-- via 'getHomeDirectory'. 'Synapse.Self.defaultsPath' returns the
-- tilde-prefixed form for pure code; this is the IO-side expansion.
absDefaultsPath :: Text -> IO FilePath
absDefaultsPath backend = do
  home <- getHomeDirectory
  pure (home </> ".plexus" </> T.unpack backend </> "defaults.json")

-- | Read a backend's defaults file.
--
-- * Missing file → 'emptyStoredDefaults' (NOT an error — absence means
--   "no defaults configured for this backend").
-- * Parse failure → 'IOException' naming the file path and the decoder's
--   message.
-- * Other IO errors (permission denied, …) → propagate as-is so the caller
--   gets a clear platform error.
loadDefaults :: Text -> IO StoredDefaults
loadDefaults backend = do
  path <- absDefaultsPath backend
  exists <- doesFileExist path
  if not exists
    then pure emptyStoredDefaults
    else do
      result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case result of
        Left ioErr
          | isDoesNotExistError ioErr -> pure emptyStoredDefaults
          | otherwise ->
              throwIO $ userError $
                "loadDefaults: failed to read " <> path
                <> ": " <> ioeGetErrorString ioErr
        Right bs -> case decodeDefaults bs of
          Left err ->
            throwIO $ userError $
              "loadDefaults: failed to parse " <> path
              <> ": " <> T.unpack err
          Right sd -> pure sd

-- ============================================================================
-- resolveAll
-- ============================================================================

-- | Resolve every 'CredentialRef' in a 'StoredDefaults' through the
-- registry. Dispatch is sequential: the first failure short-circuits and
-- the partial map is discarded — we never send a half-authenticated
-- request.
--
-- The 'MethodPath' is threaded unchanged for forward compatibility with
-- scope matching. v1 ignores it; backend-level defaults are the only ones
-- consulted.
--
-- Returns 'Right' with a fully-populated 'ResolvedDefaults' on success,
-- 'Left' with the first 'ResolveError' encountered on failure.
resolveAll
  :: ResolverRegistry
  -> StoredDefaults
  -> MethodPath
  -> IO (Either ResolveError ResolvedDefaults)
resolveAll reg StoredDefaults{..} _methodPath = do
  eCookies <- resolveMap reg sdCookies
  case eCookies of
    Left err -> pure (Left err)
    Right cookies -> do
      eHeaders <- resolveMap reg sdHeaders
      case eHeaders of
        Left err -> pure (Left err)
        Right headers -> pure $ Right ResolvedDefaults
          { rdCookies = cookies
          , rdHeaders = headers
          }

-- | Resolve every value in a @Map Text CredentialRef@, short-circuiting
-- on the first failure. Preserves input keys.
resolveMap
  :: ResolverRegistry
  -> Map Text CredentialRef
  -> IO (Either ResolveError (Map Text Text))
resolveMap reg = go . Map.toAscList
  where
    go [] = pure (Right Map.empty)
    go ((k, ref) : rest) = do
      r <- resolveRef reg ref
      case r of
        Left err -> pure (Left err)
        Right v -> do
          rest' <- go rest
          case rest' of
            Left err -> pure (Left err)
            Right m -> pure $ Right (Map.insert k v m)

-- ============================================================================
-- merge
-- ============================================================================

-- | Pure left-biased merge of CLI cookies\/headers over the resolved
-- stored defaults. CLI wins per key.
--
-- The caller is responsible for wrapping each CLI @--cookie KEY=VALUE@ and
-- @--header KEY=VALUE@ into the 'Cookies'\/'Headers' maps before calling
-- 'merge'; there is no special casing for the source of a value inside
-- this function.
merge
  :: ResolvedDefaults
  -> Cookies  -- ^ from CLI flags (wins on conflict)
  -> Headers  -- ^ from CLI flags (wins on conflict)
  -> (Cookies, Headers)
merge ResolvedDefaults{..} cliCookies cliHeaders =
  ( Map.union cliCookies rdCookies
  , Map.union cliHeaders rdHeaders
  )

-- ============================================================================
-- writeDefaults (SELF-4 stub; SELF-5 tightens)
-- ============================================================================

-- | Write a 'StoredDefaults' to disk for @backend@.
--
-- __Stub semantics (SELF-4):__ plain 'BS.writeFile' after
-- 'createDirectoryIfMissing' on the parent. No atomicity, no @chmod 600@.
-- SELF-5 replaces the body with atomic-write + mode-tightening while
-- keeping this signature intact so @_self@ command handlers need no
-- change at that time.
--
-- Callers must not rely on partial-write behavior here: the SELF-5
-- version will be atomic, and code written against this stub that
-- assumes otherwise will silently break when SELF-5 lands.
writeDefaults :: Text -> StoredDefaults -> IO ()
writeDefaults backend sd = do
  path <- absDefaultsPath backend
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (encodeDefaults sd)
