{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--
-- == Platform notes
--
-- POSIX mode semantics (@chmod 0600@ on sensitive files, @0700@ on the
-- parent dir, permission-sniffing on read) apply only on Unix. On
-- Windows ('System.Info.os' == @"mingw32"@) those steps are skipped —
-- the atomic temp-file + rename dance still happens, but permission
-- enforcement is a platform gap tracked by the SELF-5 ticket's \"Out of
-- scope\" list (no ACL story yet).
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

    -- * Write path (atomic rename + mode-aware chmod; SELF-5)
  , writeDefaults
  , absDefaultsPath
  ) where

import Control.Exception (IOException, bracketOnError, throwIO, try)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , removeFile
  , renameFile
  )
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, userError)
import qualified System.Info
import System.Posix.Files (FileStatus, fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)
import Data.Bits ((.&.))

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
--
-- __SELF-5:__ after a successful decode, inspect the file contents and
-- its Unix mode. If the encoded bytes contain a @literal:@ substring
-- (i.e. a raw credential value lives in the file) AND the mode has any
-- group- or world-accessible bits set (@mode .&. 0o077 \/= 0@), emit a
-- stderr WARN line recommending @chmod 600 \<path\>@. The read still
-- succeeds — the warning is a nudge, not a failure.
--
-- Files that contain only @keychain:\/\/@ \/ @env:\/\/@ \/ @file:\/\/@
-- refs are strictly a manifest and are safe at any mode; no warning is
-- emitted for those regardless of permissions. On Windows the mode
-- check is skipped entirely (no POSIX mode semantics).
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
          Right sd -> do
            warnIfInsecureMode path bs
            pure sd

-- | Emit a stderr WARN when the file contains a @literal:@ value AND
-- its mode is group-\/world-accessible. Silent on Windows (mode is
-- meaningless) and on non-literal-only manifests (safe at any mode).
--
-- A failure to stat the file (e.g. race with deletion) is swallowed —
-- the caller has already read the bytes, so there is no state to
-- recover from here.
warnIfInsecureMode :: FilePath -> BS.ByteString -> IO ()
warnIfInsecureMode path bs
  | System.Info.os == "mingw32" = pure ()
  | not (BS.isInfixOf "literal:" bs) = pure ()
  | otherwise = do
      mSt <- try (getFileStatus path) :: IO (Either IOException FileStatus)
      case mSt of
        Left _ -> pure ()
        Right st -> do
          let m = fileMode st
          if m .&. (0o077 :: FileMode) /= 0
            then hPutStrLn stderr $
                   "[WARN] " <> path <> " contains literal: credentials "
                   <> "and is group/world accessible; recommend `chmod 600 "
                   <> path <> "`."
            else pure ()

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
-- writeDefaults (SELF-5: atomic rename + mode-aware chmod)
-- ============================================================================

-- | Write a 'StoredDefaults' to disk for @backend@.
--
-- The write is atomic and mode-aware:
--
-- 1. Ensure the parent directory exists. If it was freshly created by
--    this call, chmod it to @0700@. If it already existed with
--    group-\/world-accessible bits set, emit a stderr WARN but leave it
--    alone (the user may have structure we shouldn\'t disturb).
-- 2. Encode the 'StoredDefaults' to JSON.
-- 3. Write the encoded bytes to a sibling temp file
--    @\<dir\>\/.defaults.json.tmp@ (hidden dotfile so visually
--    distinct if ever stuck).
-- 4. If the encoded content contains the substring @"literal:"@ anywhere,
--    chmod the temp file to @0600@ before rename. Otherwise @0644@ is
--    acceptable (manifest-only, no secrets in-file).
-- 5. Atomically 'renameFile' the temp into place — this wraps the
--    platform @rename(2)@ syscall and is the only step that makes the
--    write visible to other readers.
-- 6. On any failure at steps 3-5, remove the temp file (if it exists)
--    and propagate the original exception. No partial @defaults.json@
--    is ever left behind.
--
-- On Windows (@System.Info.os == "mingw32"@) the chmod steps are
-- skipped; the atomic rename still happens.
writeDefaults :: Text -> StoredDefaults -> IO ()
writeDefaults backend sd = do
  path <- absDefaultsPath backend
  let dir     = takeDirectory path
      tmpPath = dir </> ".defaults.json.tmp"
      bytes   = encodeDefaults sd
      sensitive = BS.isInfixOf "literal:" bytes

  -- Step 1: parent dir. Track whether we created it so we only chmod
  -- freshly-created dirs (existing dirs with wide mode get a WARN).
  dirPreexisted <- doesDirectoryExist dir
  createDirectoryIfMissing True dir
  if not dirPreexisted
    then setFileModeUnix dir 0o700
    else warnIfWideDir dir

  -- Steps 2-5: atomic temp write + chmod + rename. `bracketOnError`
  -- removes the temp file on any exception from the write or rename;
  -- on success no cleanup runs.
  bracketOnError
    (pure tmpPath)
    (\p -> E.handle (\(_ :: IOException) -> pure ()) (removeFileIfExists p))
    (\p -> do
        BS.writeFile p bytes
        setFileModeUnix p (if sensitive then 0o600 else 0o644)
        renameFile p path)

-- | Remove a file, ignoring "does not exist" errors but propagating
-- anything else. Used by the 'bracketOnError' cleanup — we don\'t want
-- cleanup-of-nonexistent-tmp to hide the real failure.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists p = do
  exists <- doesFileExist p
  if exists then removeFile p else pure ()

-- | 'setFileMode' on Unix, no-op on Windows. Matches the platform gating
-- in 'warnIfInsecureMode' / 'warnIfWideDir'.
setFileModeUnix :: FilePath -> FileMode -> IO ()
setFileModeUnix p m
  | System.Info.os == "mingw32" = pure ()
  | otherwise                   = setFileMode p m

-- | WARN when a pre-existing parent dir has group-\/world-accessible
-- bits. We never tighten an existing dir\'s mode — users may have
-- structure under @~\/.plexus@ we shouldn\'t perturb. Silent on
-- Windows and on already-tight dirs.
warnIfWideDir :: FilePath -> IO ()
warnIfWideDir dir
  | System.Info.os == "mingw32" = pure ()
  | otherwise = do
      mSt <- try (getFileStatus dir) :: IO (Either IOException FileStatus)
      case mSt of
        Left _ -> pure ()
        Right st -> do
          let m = fileMode st
          if m .&. (0o077 :: FileMode) /= 0
            then hPutStrLn stderr $
                   "[WARN] " <> dir <> " has group/world access bits set; "
                   <> "recommend `chmod 700 " <> dir <> "` to tighten."
            else pure ()
