{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the SELF-3 legacy token migration in 'Synapse.Self.IO':
--
-- * Legacy @~\/.plexus\/tokens\/\<backend\>@ present, no new
--   @defaults.json@ → on 'loadDefaults': legacy removed, new file written
--   with @cookies.access_token = \"literal:\<jwt\>\"@, returned
--   'StoredDefaults' carries that ref, file mode is @0600@, INFO logged.
-- * Legacy + new both present → legacy removed unread, new file
--   untouched, returned 'StoredDefaults' matches the new file.
-- * Neither file → 'emptyStoredDefaults', no files created.
-- * Empty legacy file → deleted silently, no new file created.
-- * Whitespace-only legacy → deleted silently, no new file created.
-- * INFO log line matches the expected wording (including the
--   \"Consider: synapse _self\" upgrade hint).
--
-- Every test isolates filesystem state via 'withSystemTempDirectory' +
-- a bracketed @setEnv "HOME" tmp@. No OS keychain, no external process
-- invocation. This spec follows the SELF-5 test pattern.
module SelfMigrationSpec (spec) where

import Control.Exception (bracket, bracket_)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits ((.&.))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import qualified System.Info
import System.IO (IOMode(..), hClose, hFlush, openFile, stderr)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Types (FileMode)
import Test.Hspec

import Synapse.Self
  ( CredentialRef(..)
  , StoredDefaults(..)
  , emptyStoredDefaults
  , encodeDefaults
  , loadDefaults
  )

-- | True on Unix-style platforms where POSIX modes have meaning.
isUnix :: Bool
isUnix = System.Info.os /= "mingw32"

-- | File mode masked to the permission bits (drop file-type bits).
permBits :: FileMode -> FileMode
permBits m = m .&. 0o777

-- | Run @act@ with @HOME@ pointed at @tmp@, restoring the prior @HOME@
-- (or unsetting it if there was none) after.
withHome :: FilePath -> IO a -> IO a
withHome tmp act = do
  prior <- lookupEnv "HOME"
  bracket_
    (setEnv "HOME" tmp)
    (case prior of
       Just v  -> setEnv "HOME" v
       Nothing -> unsetEnv "HOME")
    act

-- | Capture whatever is written to stderr during @act@. Writes to a
-- fresh file under the supplied scratch dir. Restores the original
-- stderr — even if @act@ throws — before reading the captured bytes
-- back.
captureStderrInto :: FilePath -> IO a -> IO (a, String)
captureStderrInto scratchDir act = do
  let capPath = scratchDir </> "captured-stderr.txt"
  hFlush stderr
  origStderr <- hDuplicate stderr
  r <- bracket
    (openFile capPath WriteMode)
    (\h -> do
        hFlush stderr
        hDuplicateTo origStderr stderr
        hClose origStderr
        hClose h)
    (\h -> do
        hDuplicateTo h stderr
        act)
  bs <- BS.readFile capPath
  let captured = T.unpack
        (T.pack (map (toEnum . fromIntegral) (BS.unpack bs)))
  pure (r, captured)

-- | Backend name used across these specs. Unique suffix so we never
-- collide with the developer's real @~\/.plexus\/@ tree even if the
-- HOME redirection ever failed.
testBackend :: Text
testBackend = "self3-test-bk"

-- | Place @bytes@ at @tmp/.plexus/tokens/\<backend\>@, creating the
-- parent directory if needed.
placeLegacy :: FilePath -> Text -> BS.ByteString -> IO ()
placeLegacy tmp backend bytes = do
  let tokensDir = tmp </> ".plexus" </> "tokens"
  createDirectoryIfMissing True tokensDir
  BS.writeFile (tokensDir </> T.unpack backend) bytes

-- | Absolute path to the legacy token file under the sandboxed HOME.
legacyPath :: FilePath -> Text -> FilePath
legacyPath tmp backend = tmp </> ".plexus" </> "tokens" </> T.unpack backend

-- | Absolute path to the new defaults.json under the sandboxed HOME.
newPath :: FilePath -> Text -> FilePath
newPath tmp backend =
  tmp </> ".plexus" </> T.unpack backend </> "defaults.json"

-- | True when @needle@ is a substring of @haystack@.
containsSubstring :: String -> String -> Bool
containsSubstring needle haystack =
  T.isInfixOf (T.pack needle) (T.pack haystack)

-- | A realistic-looking JWT for the migrated value (shape only).
sampleJwt :: Text
sampleJwt = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0In0.sig"

spec :: Spec
spec = do
  describe "SELF-3 legacy token migration" $ do

    it "migrates a legacy token to literal: ref and removes legacy" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend (TE.encodeUtf8 sampleJwt)
          (loaded, _err) <- captureStderrInto tmp $ loadDefaults testBackend
          -- Legacy removed.
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          -- New file exists with the expected content.
          doesFileExist (newPath tmp testBackend) `shouldReturn` True
          bs <- BS.readFile (newPath tmp testBackend)
          BS8.unpack bs `shouldSatisfy`
            containsSubstring ("literal:" <> T.unpack sampleJwt)
          -- Returned defaults match.
          sdCookies loaded `shouldBe`
            Map.singleton "access_token"
              (CredentialRef ("literal:" <> sampleJwt))
          sdHeaders loaded `shouldBe` Map.empty

    it "trims surrounding whitespace from the legacy token" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend
            (TE.encodeUtf8 ("  \n" <> sampleJwt <> "\n  "))
          (loaded, _err) <- captureStderrInto tmp $ loadDefaults testBackend
          sdCookies loaded `shouldBe`
            Map.singleton "access_token"
              (CredentialRef ("literal:" <> sampleJwt))

    it "writes the migrated defaults.json with mode 0600 (literal: content)" $
      if not isUnix
        then pendingWith "POSIX-only semantics"
        else withSystemTempDirectory "synapse-self3" $ \tmp ->
          withHome tmp $ do
            placeLegacy tmp testBackend (TE.encodeUtf8 sampleJwt)
            _ <- captureStderrInto tmp $ loadDefaults testBackend
            st <- getFileStatus (newPath tmp testBackend)
            permBits (fileMode st) `shouldBe` 0o600

    it "deletes the legacy file unread when the new file already exists" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          -- Stage an authoritative new defaults.json first.
          let existing = emptyStoredDefaults
                { sdCookies = Map.fromList
                    [ ("session", CredentialRef "literal:from-new-file") ]
                }
              newDir = tmp </> ".plexus" </> T.unpack testBackend
          createDirectoryIfMissing True newDir
          BS.writeFile (newPath tmp testBackend) (encodeDefaults existing)
          -- Stage a legacy file too — must be ignored.
          placeLegacy tmp testBackend (TE.encodeUtf8 "ignored-legacy-jwt")

          (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
          -- Legacy removed.
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          -- New file preserved byte-for-byte.
          bsAfter <- BS.readFile (newPath tmp testBackend)
          bsAfter `shouldBe` encodeDefaults existing
          -- Returned defaults = new file contents (NOT the legacy jwt).
          loaded `shouldBe` existing
          -- INFO line names the stale legacy file.
          err `shouldSatisfy` containsSubstring "[INFO]"
          err `shouldSatisfy` containsSubstring "stale legacy token file"
          err `shouldSatisfy`
            containsSubstring (legacyPath tmp testBackend)

    it "returns empty defaults and creates no files when neither exists" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          (loaded, _err) <- captureStderrInto tmp $ loadDefaults testBackend
          loaded `shouldBe` emptyStoredDefaults
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          doesFileExist (newPath tmp testBackend) `shouldReturn` False
          -- Backend subdir is not created on the empty path.
          doesDirectoryExist (tmp </> ".plexus" </> T.unpack testBackend)
            `shouldReturn` False

    it "silently deletes an empty legacy file without creating a new one" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend BS.empty
          (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
          loaded `shouldBe` emptyStoredDefaults
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          doesFileExist (newPath tmp testBackend) `shouldReturn` False
          -- Silent: no INFO line about migration on an empty file.
          err `shouldNotSatisfy` containsSubstring "migrated legacy"

    it "silently deletes a whitespace-only legacy file without creating a new one" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend (BS8.pack "   \n\t\n  ")
          (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
          loaded `shouldBe` emptyStoredDefaults
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          doesFileExist (newPath tmp testBackend) `shouldReturn` False
          err `shouldNotSatisfy` containsSubstring "migrated legacy"

    it "emits an INFO line with the full migration hint wording" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend (TE.encodeUtf8 sampleJwt)
          (_loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
          err `shouldSatisfy` containsSubstring "[INFO]"
          err `shouldSatisfy` containsSubstring "migrated legacy token file"
          err `shouldSatisfy`
            containsSubstring (legacyPath tmp testBackend)
          err `shouldSatisfy`
            containsSubstring (newPath tmp testBackend)
          err `shouldSatisfy` containsSubstring "(stored as literal:)"
          err `shouldSatisfy` containsSubstring "Consider: synapse _self "
          err `shouldSatisfy` containsSubstring (T.unpack testBackend)
          err `shouldSatisfy` containsSubstring "upgrade-to-keychain"

    it "is idempotent: running loadDefaults twice yields the same result" $
      withSystemTempDirectory "synapse-self3" $ \tmp ->
        withHome tmp $ do
          placeLegacy tmp testBackend (TE.encodeUtf8 sampleJwt)
          (loaded1, _err1) <- captureStderrInto tmp $ loadDefaults testBackend
          -- After the first call, the legacy file is gone. The second
          -- call sees only the new defaults.json and returns the same
          -- StoredDefaults. This exercises the "new file absent, legacy
          -- absent" fast path as a follow-up to migration.
          doesFileExist (legacyPath tmp testBackend) `shouldReturn` False
          (loaded2, _err2) <- captureStderrInto tmp $ loadDefaults testBackend
          loaded2 `shouldBe` loaded1
