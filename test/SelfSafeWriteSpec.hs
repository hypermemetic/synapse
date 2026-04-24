{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the SELF-5 safe-write path in 'Synapse.Self.IO':
--
-- * Atomic rename via temp file (@.defaults.json.tmp@).
-- * Content-aware chmod (@0600@ with @literal:@ values, @0644@ without).
-- * Parent directory created mode @0700@ when freshly created.
-- * Pre-existing wide parent directory → WARN, dir untouched.
-- * Simulated mid-write failure → temp file cleaned up, no partial
--   @defaults.json@.
-- * 'loadDefaults' permission sniffing: WARN on world-readable files
--   with @literal:@ content; silent on manifest-only files regardless
--   of mode.
--
-- Every test isolates filesystem state via 'withSystemTempDirectory' +
-- a bracketed @setEnv "HOME" tmp@. No OS keychain, no external process
-- invocation. Mode checks are skipped on Windows (POSIX-only semantics).
module SelfSafeWriteSpec (spec) where

import Control.Exception (bracket, bracket_, try)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.Bits ((.&.))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import qualified System.Info
import System.IO (IOMode(..), hClose, hFlush, openFile, stderr)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)
import Test.Hspec

import Synapse.Self
  ( CredentialRef(..)
  , StoredDefaults(..)
  , emptyStoredDefaults
  , loadDefaults
  , writeDefaults
  )

-- | True on Unix-style platforms where POSIX modes have meaning.
isUnix :: Bool
isUnix = System.Info.os /= "mingw32"

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
-- fresh file under the supplied scratch dir (caller threads one in so
-- capture output stays inside the test\'s temp-dir sandbox). Restores
-- the original stderr — even if @act@ throws — before reading the
-- captured bytes back.
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

-- | Backend name used across these specs — unique suffix so we never
-- collide with the developer\'s real @~\/.plexus\/@ tree even if the
-- HOME redirection ever failed.
testBackend :: Text
testBackend = "self5-test-bk"

-- | File mode masked to the permission bits (drop file-type bits).
permBits :: FileMode -> FileMode
permBits m = m .&. 0o777

spec :: Spec
spec = do
  describe "SELF-5 safe write" $ do

    -- ========================================================================
    -- writeDefaults: mode-aware chmod
    -- ========================================================================
    describe "chmod policy" $ do
      it "sets 0600 when any value starts with literal:" $
        withSystemTempDirectory "synapse-self5" $ \tmp ->
          withHome tmp $ do
            let sd = emptyStoredDefaults
                  { sdCookies = Map.fromList
                      [ ("access_token", CredentialRef "literal:secret-token") ]
                  }
            writeDefaults testBackend sd
            let path = tmp </> ".plexus" </> T.unpack testBackend </> "defaults.json"
            exists <- doesFileExist path
            exists `shouldBe` True
            if isUnix
              then do
                st <- getFileStatus path
                permBits (fileMode st) `shouldBe` 0o600
              else pure ()

      it "sets 0644 when values are only non-literal refs" $
        withSystemTempDirectory "synapse-self5" $ \tmp ->
          withHome tmp $ do
            let sd = emptyStoredDefaults
                  { sdCookies = Map.fromList
                      [ ("access_token", CredentialRef "keychain://svc/tok") ]
                  , sdHeaders = Map.fromList
                      [ ("X-Key", CredentialRef "env://API_KEY") ]
                  }
            writeDefaults testBackend sd
            let path = tmp </> ".plexus" </> T.unpack testBackend </> "defaults.json"
            if isUnix
              then do
                st <- getFileStatus path
                permBits (fileMode st) `shouldBe` 0o644
              else pure ()

      it "sets 0600 when any one of several refs is literal" $
        withSystemTempDirectory "synapse-self5" $ \tmp ->
          withHome tmp $ do
            let sd = emptyStoredDefaults
                  { sdCookies = Map.fromList
                      [ ("session",  CredentialRef "keychain://svc/session")
                      , ("backup",   CredentialRef "literal:emergency-value")
                      ]
                  }
            writeDefaults testBackend sd
            let path = tmp </> ".plexus" </> T.unpack testBackend </> "defaults.json"
            if isUnix
              then do
                st <- getFileStatus path
                permBits (fileMode st) `shouldBe` 0o600
              else pure ()

    -- ========================================================================
    -- writeDefaults: parent directory mode
    -- ========================================================================
    describe "parent directory" $ do
      it "creates a fresh parent directory with mode 0700" $
        withSystemTempDirectory "synapse-self5" $ \tmp ->
          withHome tmp $ do
            writeDefaults testBackend emptyStoredDefaults
            let dir = tmp </> ".plexus" </> T.unpack testBackend
            if isUnix
              then do
                st <- getFileStatus dir
                permBits (fileMode st) `shouldBe` 0o700
              else pure ()

      it "leaves an existing wide parent directory untouched and WARNs" $
        if not isUnix
          then pendingWith "POSIX-only semantics"
          else withSystemTempDirectory "synapse-self5" $ \tmp ->
            withHome tmp $ do
              let dir = tmp </> ".plexus" </> T.unpack testBackend
              createDirectoryIfMissing True dir
              setFileMode dir 0o755  -- intentionally wide
              (_, err) <- captureStderrInto tmp $ writeDefaults testBackend emptyStoredDefaults
              st <- getFileStatus dir
              -- Mode untouched.
              permBits (fileMode st) `shouldBe` 0o755
              -- WARN emitted.
              err `shouldSatisfy` containsSubstring "[WARN]"
              err `shouldSatisfy` containsSubstring dir

    -- ========================================================================
    -- writeDefaults: atomicity
    -- ========================================================================
    describe "atomicity" $ do
      it "leaves no temp file and no partial defaults.json when the write fails" $
        if not isUnix
          then pendingWith "needs POSIX mode to stage the failure"
          else withSystemTempDirectory "synapse-self5" $ \tmp ->
            withHome tmp $ do
              let dir     = tmp </> ".plexus" </> T.unpack testBackend
                  tmpFile = dir </> ".defaults.json.tmp"
                  path    = dir </> "defaults.json"
              createDirectoryIfMissing True dir
              -- Make the parent dir read-only so writeFile to the tmp
              -- path fails. Restore 0700 in the cleanup so the temp
              -- directory teardown itself succeeds.
              bracket_
                (setFileMode dir 0o500)
                (setFileMode dir 0o700)
                $ do
                  let sd = emptyStoredDefaults
                        { sdCookies = Map.fromList
                            [ ("t", CredentialRef "literal:foo") ]
                        }
                  r <- try (writeDefaults testBackend sd)
                    :: IO (Either E.SomeException ())
                  case r of
                    Left _  -> pure ()  -- expected: write failed
                    Right _ -> expectationFailure
                      "writeDefaults should have failed on a read-only parent dir"
                  -- Neither artifact should exist after the failure.
                  doesFileExist tmpFile `shouldReturn` False
                  doesFileExist path    `shouldReturn` False

      it "rename is atomic: final file appears only after a complete write" $
        withSystemTempDirectory "synapse-self5" $ \tmp ->
          withHome tmp $ do
            let sd = emptyStoredDefaults
                  { sdCookies = Map.fromList
                      [ ("t", CredentialRef "literal:value") ]
                  }
                dir     = tmp </> ".plexus" </> T.unpack testBackend
                tmpFile = dir </> ".defaults.json.tmp"
                path    = dir </> "defaults.json"
            writeDefaults testBackend sd
            -- After a successful write, the temp file is gone and the
            -- final file exists.
            doesFileExist tmpFile `shouldReturn` False
            doesFileExist path    `shouldReturn` True

    -- ========================================================================
    -- loadDefaults: permission sniffing
    -- ========================================================================
    describe "loadDefaults permission sniffing" $ do
      it "WARNs on world-readable file with literal: content" $
        if not isUnix
          then pendingWith "POSIX-only semantics"
          else withSystemTempDirectory "synapse-self5" $ \tmp ->
            withHome tmp $ do
              let sd = emptyStoredDefaults
                    { sdCookies = Map.fromList
                        [ ("t", CredentialRef "literal:oops-wide-open") ]
                    }
                  path = tmp </> ".plexus" </> T.unpack testBackend </> "defaults.json"
              writeDefaults testBackend sd
              -- Intentionally loosen the file to simulate a user who
              -- chmod-ed it by hand.
              setFileMode path 0o644
              (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
              loaded `shouldBe` sd
              err `shouldSatisfy` containsSubstring "[WARN]"
              err `shouldSatisfy` containsSubstring path
              err `shouldSatisfy` containsSubstring "chmod 600"

      it "does NOT WARN on world-readable file with only non-literal refs" $
        if not isUnix
          then pendingWith "POSIX-only semantics"
          else withSystemTempDirectory "synapse-self5" $ \tmp ->
            withHome tmp $ do
              let sd = emptyStoredDefaults
                    { sdCookies = Map.fromList
                        [ ("t", CredentialRef "keychain://svc/token") ]
                    , sdHeaders = Map.fromList
                        [ ("X", CredentialRef "env://API_KEY") ]
                    }
                  path = tmp </> ".plexus" </> T.unpack testBackend </> "defaults.json"
              writeDefaults testBackend sd
              setFileMode path 0o644  -- wide, but file is manifest-only
              (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
              loaded `shouldBe` sd
              err `shouldNotSatisfy` containsSubstring "[WARN]"

      it "does NOT WARN on a tight (0600) file with literal: content" $
        if not isUnix
          then pendingWith "POSIX-only semantics"
          else withSystemTempDirectory "synapse-self5" $ \tmp ->
            withHome tmp $ do
              let sd = emptyStoredDefaults
                    { sdCookies = Map.fromList
                        [ ("t", CredentialRef "literal:tight-and-tidy") ]
                    }
              writeDefaults testBackend sd
              -- writeDefaults already set 0600 on this file.
              (loaded, err) <- captureStderrInto tmp $ loadDefaults testBackend
              loaded `shouldBe` sd
              err `shouldNotSatisfy` containsSubstring "[WARN]"

-- | True when @needle@ is a substring of @haystack@.
containsSubstring :: String -> String -> Bool
containsSubstring needle haystack =
  T.isInfixOf (T.pack needle) (T.pack haystack)
