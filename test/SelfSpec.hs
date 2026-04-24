{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for Synapse.Self (SELF-1, SELF-7).
--
-- Covers:
--   - StoredDefaults roundtrip encode/decode with mixed URI schemes
--   - Version mismatch rejection
--   - Unknown top-level field tolerance (forward compatibility)
--   - Missing "defaults" object treated as empty
--   - parseUri shape coverage (literal, env, keychain, file, bare-string reject)
--   - Deterministic key ordering from encodeDefaults
--   - Empty resolver registry returns ResolveUnknownScheme
--   - SELF-7: literalResolver, envResolver, fileResolver, defaultRegistry
module Main where

import Control.Exception (bracket, bracket_, finally)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory
  ( getHomeDirectory
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import qualified System.IO as IO
import Test.Hspec

import Synapse.Self

main :: IO ()
main = hspec $ do
  describe "defaultsPath" $ do
    it "returns ~/.plexus/<backend>/defaults.json" $ do
      defaultsPath "substrate" `shouldBe` "~/.plexus/substrate/defaults.json"

    it "handles backends with dashes" $ do
      defaultsPath "my-backend" `shouldBe` "~/.plexus/my-backend/defaults.json"

  describe "parseUri" $ do
    it "parses literal: as opaque body" $ do
      parseUri (CredentialRef "literal:abc123") `shouldBe`
        Right (ParsedUri "literal" (OpaqueBody "abc123"))

    it "preserves colons in opaque bodies (e.g. JWTs)" $ do
      parseUri (CredentialRef "literal:eyJhbGciOi.JJWT.sig") `shouldBe`
        Right (ParsedUri "literal" (OpaqueBody "eyJhbGciOi.JJWT.sig"))

    it "parses env://VAR" $ do
      parseUri (CredentialRef "env://USCIS_API_KEY") `shouldBe`
        Right (ParsedUri "env" (HierarchicalBody "USCIS_API_KEY" "" []))

    it "parses keychain://service/account" $ do
      parseUri (CredentialRef "keychain://uscis/access_token") `shouldBe`
        Right (ParsedUri "keychain" (HierarchicalBody "uscis" "/access_token" []))

    it "parses file:///abs/path" $ do
      parseUri (CredentialRef "file:///etc/plexus/token") `shouldBe`
        Right (ParsedUri "file" (HierarchicalBody "" "/etc/plexus/token" []))

    it "parses hierarchical URIs with query strings" $ do
      parseUri (CredentialRef "env://FOO?fallback=bar&retry=3") `shouldBe`
        Right (ParsedUri "env"
          (HierarchicalBody "FOO" "" [("fallback", "bar"), ("retry", "3")]))

    it "rejects bare strings with no scheme" $ do
      case parseUri (CredentialRef "abc123") of
        Left _  -> pure ()
        Right p -> expectationFailure ("expected Left, got Right " <> show p)

    it "rejects empty scheme" $ do
      case parseUri (CredentialRef ":foo") of
        Left _  -> pure ()
        Right p -> expectationFailure ("expected Left, got Right " <> show p)

  describe "encodeDefaults / decodeDefaults" $ do
    let mixed = StoredDefaults
          { sdVersion = 1
          , sdCookies = Map.fromList
              [ ("access_token", CredentialRef "keychain://uscis/access_token")
              , ("session",      CredentialRef "literal:raw-session-cookie")
              ]
          , sdHeaders = Map.fromList
              [ ("X-Trace-Id", CredentialRef "literal:abc123")
              , ("X-API-Key",  CredentialRef "env://USCIS_API_KEY")
              , ("X-Token",    CredentialRef "file:///etc/plexus/token")
              ]
          , sdScopes = Map.empty
          }

    it "roundtrips a file with mixed URI schemes" $ do
      let encoded = encodeDefaults mixed
      decodeDefaults encoded `shouldBe` Right mixed

    it "produces deterministic output across calls" $ do
      encodeDefaults mixed `shouldBe` encodeDefaults mixed

    it "uses two-space indentation" $ do
      let encoded = TE.decodeUtf8 (encodeDefaults mixed)
      -- Nested fields should be indented with exactly 2 spaces per level.
      -- We check that no tab characters appear and that indented lines
      -- begin with 2-space multiples.
      T.any (== '\t') encoded `shouldBe` False
      let firstIndent = dropWhile (/= '\n') (T.unpack encoded)
      take 3 (drop 1 firstIndent) `shouldBe` "  \""

    it "orders keys deterministically (ascending)" $ do
      let encoded = TE.decodeUtf8 (encodeDefaults mixed)
      -- "cookies" should appear before "headers", and within cookies
      -- "access_token" before "session".
      let cookiesIx     = T.breakOn "cookies" encoded
          headersIx     = T.breakOn "headers" encoded
          accessIx      = T.breakOn "access_token" encoded
          sessionIx     = T.breakOn "session" encoded
      T.length (fst cookiesIx) `shouldSatisfy` (< T.length (fst headersIx))
      T.length (fst accessIx)  `shouldSatisfy` (< T.length (fst sessionIx))

    it "treats missing defaults object as empty" $ do
      let raw = BS8.pack "{\"version\": 1}"
      case decodeDefaults raw of
        Right sd -> do
          sdVersion sd `shouldBe` 1
          sdCookies sd `shouldBe` Map.empty
          sdHeaders sd `shouldBe` Map.empty
          sdScopes  sd `shouldBe` Map.empty
        Left err -> expectationFailure ("unexpected error: " <> T.unpack err)

    it "tolerates unknown top-level fields (forward compat)" $ do
      let raw = BS8.pack
            "{\"version\": 1, \"future_feature\": {\"some\": \"thing\"}, \
            \\"defaults\": {\"cookies\": {\"c\": \"literal:v\"}}}"
      case decodeDefaults raw of
        Right sd ->
          Map.lookup "c" (sdCookies sd) `shouldBe` Just (CredentialRef "literal:v")
        Left err -> expectationFailure ("unexpected error: " <> T.unpack err)

    it "tolerates unknown fields inside the defaults object" $ do
      let raw = BS8.pack
            "{\"version\": 1, \"defaults\": \
            \{\"cookies\": {}, \"headers\": {}, \"bonus\": {}}}"
      decodeDefaults raw `shouldSatisfy` isRight

    it "rejects unknown versions with structured message" $ do
      let raw = BS8.pack "{\"version\": 2, \"defaults\": {}}"
      decodeDefaults raw `shouldBe` Left "unsupported version: 2, expected 1"

    it "rejects version 0 with structured message" $ do
      let raw = BS8.pack "{\"version\": 0}"
      decodeDefaults raw `shouldBe` Left "unsupported version: 0, expected 1"

    it "decodes scopes even though v1 ignores them semantically" $ do
      let raw = BS8.pack
            "{\"version\": 1, \"scopes\": {\"cone\": {\"cookies\": {\"x\": \"literal:v\"}}}}"
      case decodeDefaults raw of
        Right sd -> do
          case Map.lookup "cone" (sdScopes sd) of
            Just scoped ->
              Map.lookup "x" (sdsCookies scoped) `shouldBe` Just (CredentialRef "literal:v")
            Nothing -> expectationFailure "expected cone scope"
        Left err -> expectationFailure ("unexpected error: " <> T.unpack err)

  describe "ResolverRegistry" $ do
    it "mempty returns ResolveUnknownScheme for any scheme" $ do
      let reg = mempty :: ResolverRegistry
      result <- resolveRef reg (CredentialRef "literal:foo")
      result `shouldBe` Left (ResolveUnknownScheme "literal")

    it "mempty reports the correct scheme name on lookup failure" $ do
      let reg = mempty :: ResolverRegistry
      result <- resolveRef reg (CredentialRef "keychain://svc/account")
      result `shouldBe` Left (ResolveUnknownScheme "keychain")

    it "surfaces parse errors via ResolveParseError" $ do
      let reg = mempty :: ResolverRegistry
      result <- resolveRef reg (CredentialRef "no-scheme-here")
      case result of
        Left (ResolveParseError _) -> pure ()
        other -> expectationFailure ("expected ResolveParseError, got " <> show other)

    it "dispatches to a registered resolver" $ do
      let reg = registerResolver "literal"
                  (\p -> pure $ Right $ case puBody p of
                     OpaqueBody v -> v
                     _            -> "unexpected-body")
                  mempty
      result <- resolveRef reg (CredentialRef "literal:hello")
      result `shouldBe` Right "hello"

    it "later registrations override earlier ones" $ do
      let reg = registerResolver "s" (\_ -> pure (Right "second"))
              $ registerResolver "s" (\_ -> pure (Right "first"))
                  mempty
      result <- resolveRef reg (CredentialRef "s://anything")
      result `shouldBe` Right "second"

  -- ==========================================================================
  -- SELF-7: core resolvers
  -- ==========================================================================

  describe "literalResolver" $ do
    let reg = registerResolver "literal" literalResolver mempty

    it "returns the body verbatim for literal:foo" $ do
      result <- resolveRef reg (CredentialRef "literal:foo")
      result `shouldBe` Right "foo"

    it "returns empty string for literal: (empty body)" $ do
      result <- resolveRef reg (CredentialRef "literal:")
      result `shouldBe` Right ""

    it "preserves leading/trailing spaces (literal: foo )" $ do
      result <- resolveRef reg (CredentialRef "literal: foo ")
      result `shouldBe` Right " foo "

    it "preserves interior colons (JWT-style)" $ do
      result <- resolveRef reg (CredentialRef "literal:eyJhbGciOi.JJWT.sig")
      result `shouldBe` Right "eyJhbGciOi.JJWT.sig"

    it "rejects hierarchical literal:// as backend error" $ do
      result <- resolveRef reg (CredentialRef "literal://foo")
      case result of
        Left (ResolveBackendError _ _) -> pure ()
        other -> expectationFailure
          ("expected ResolveBackendError, got " <> show other)

  describe "envResolver" $ do
    let reg = registerResolver "env" envResolver mempty
        testVar = "SYNAPSE_SELF_TEST_VAR_7F3A"

    it "returns the variable value when set" $
      withEnv testVar "hello-env" $ do
        result <- resolveRef reg (CredentialRef ("env://" <> T.pack testVar))
        result `shouldBe` Right "hello-env"

    it "preserves whitespace inside variable values" $
      withEnv testVar " spaced " $ do
        result <- resolveRef reg (CredentialRef ("env://" <> T.pack testVar))
        result `shouldBe` Right " spaced "

    it "reports ResolveNotFound when the variable is unset" $
      withoutEnv testVar $ do
        result <- resolveRef reg (CredentialRef ("env://" <> T.pack testVar))
        result `shouldBe` Left
          (ResolveNotFound (CredentialRef ("env://" <> T.pack testVar)))

    it "reports ResolveBackendError for env:// with no variable name" $ do
      result <- resolveRef reg (CredentialRef "env://")
      case result of
        Left (ResolveBackendError _ _) -> pure ()
        other -> expectationFailure
          ("expected ResolveBackendError, got " <> show other)

    it "reports ResolveBackendError for opaque env:VAR" $ do
      result <- resolveRef reg (CredentialRef "env:FOO")
      case result of
        Left (ResolveBackendError _ _) -> pure ()
        other -> expectationFailure
          ("expected ResolveBackendError, got " <> show other)

  describe "fileResolver" $ do
    let reg = registerResolver "file" fileResolver mempty

    it "reads file contents and strips a single trailing newline" $
      withTempFileContents "hello\n" $ \path -> do
        result <- resolveRef reg
          (CredentialRef ("file://" <> T.pack path))
        result `shouldBe` Right "hello"

    it "preserves interior newlines and whitespace" $
      withTempFileContents "a\n  b  \nc\n" $ \path -> do
        result <- resolveRef reg
          (CredentialRef ("file://" <> T.pack path))
        result `shouldBe` Right "a\n  b  \nc"

    it "returns contents verbatim when there is no trailing newline" $
      withTempFileContents "no-newline" $ \path -> do
        result <- resolveRef reg
          (CredentialRef ("file://" <> T.pack path))
        result `shouldBe` Right "no-newline"

    it "strips only one trailing newline (leaves the rest)" $
      withTempFileContents "two\n\n" $ \path -> do
        result <- resolveRef reg
          (CredentialRef ("file://" <> T.pack path))
        result `shouldBe` Right "two\n"

    it "returns ResolveNotFound for a missing file" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "synapse-self-does-not-exist-7f3a"
          ref  = CredentialRef ("file://" <> T.pack path)
      result <- resolveRef reg ref
      result `shouldBe` Left (ResolveNotFound ref)

    it "returns ResolveBackendError when the path is a directory" $ do
      tmp <- getTemporaryDirectory
      result <- resolveRef reg
        (CredentialRef ("file://" <> T.pack tmp))
      case result of
        Left (ResolveBackendError _ _) -> pure ()
        other -> expectationFailure
          ("expected ResolveBackendError, got " <> show other)

    it "expands ~ to the user's home directory" $ do
      home <- getHomeDirectory
      -- Write a tmp file directly under $HOME so the ~ expansion test
      -- doesn't depend on any specific subdir layout.
      let name = ".synapse-self-test-7f3a"
          path = home </> name
      bracket_
        (TIO.writeFile path "tilde-ok\n")
        (removePathForcibly path)
        $ do
          result <- resolveRef reg
            (CredentialRef (T.pack ("file://~/" <> name)))
          result `shouldBe` Right "tilde-ok"

    it "reports ResolveBackendError for opaque file:path" $ do
      result <- resolveRef reg (CredentialRef "file:/tmp/x")
      case result of
        Left (ResolveBackendError _ _) -> pure ()
        other -> expectationFailure
          ("expected ResolveBackendError, got " <> show other)

  describe "defaultRegistry" $ do
    it "dispatches literal: via the default registry" $ do
      result <- resolveRef defaultRegistry (CredentialRef "literal:abc")
      result `shouldBe` Right "abc"

    it "dispatches env:// via the default registry" $
      withEnv "SYNAPSE_SELF_TEST_DEFAULT_REG" "reg-ok" $ do
        result <- resolveRef defaultRegistry
          (CredentialRef "env://SYNAPSE_SELF_TEST_DEFAULT_REG")
        result `shouldBe` Right "reg-ok"

    it "dispatches file:// via the default registry" $
      withTempFileContents "file-ok\n" $ \path -> do
        result <- resolveRef defaultRegistry
          (CredentialRef ("file://" <> T.pack path))
        result `shouldBe` Right "file-ok"

    it "returns ResolveUnknownScheme for unregistered schemes" $ do
      result <- resolveRef defaultRegistry
        (CredentialRef "keychain://svc/account")
      result `shouldBe` Left (ResolveUnknownScheme "keychain")

    it "end-to-end: resolves a StoredDefaults with one entry per scheme" $
      withEnv "SYNAPSE_SELF_TEST_E2E_ENV" "from-env" $
      withTempFileContents "from-file\n" $ \path -> do
        let sd = StoredDefaults
              { sdVersion = 1
              , sdCookies = Map.empty
              , sdHeaders = Map.fromList
                  [ ("X-Literal", CredentialRef "literal:from-literal")
                  , ("X-Env",     CredentialRef "env://SYNAPSE_SELF_TEST_E2E_ENV")
                  , ("X-File",    CredentialRef ("file://" <> T.pack path))
                  ]
              , sdScopes  = Map.empty
              }
        -- Resolve every header ref end-to-end via defaultRegistry.
        resolved <- traverse (resolveRef defaultRegistry) (sdHeaders sd)
        Map.lookup "X-Literal" resolved `shouldBe` Just (Right "from-literal")
        Map.lookup "X-Env"     resolved `shouldBe` Just (Right "from-env")
        Map.lookup "X-File"    resolved `shouldBe` Just (Right "from-file")

-- | Set an env var for the duration of an action, restoring any prior
-- value (or unsetting if it was unset).
withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
  prior <- lookupEnv name
  bracket_
    (setEnv name value)
    (case prior of
       Just v  -> setEnv name v
       Nothing -> unsetEnv name)
    action

-- | Ensure an env var is unset for the duration of an action, restoring
-- it afterwards.
withoutEnv :: String -> IO a -> IO a
withoutEnv name action = do
  prior <- lookupEnv name
  bracket_
    (unsetEnv name)
    (case prior of
       Just v  -> setEnv name v
       Nothing -> pure ())
    action

-- | Write @contents@ to a fresh temp file, pass its path to @action@,
-- and unlink the file on the way out.
withTempFileContents :: Text -> (FilePath -> IO a) -> IO a
withTempFileContents contents action = do
  tmpDir <- getTemporaryDirectory
  bracket
    (openTempFile tmpDir "synapse-self-test.txt")
    (\(path, h) -> hClose h `finally` removeFile path)
    (\(path, h) -> do
        IO.hSetEncoding h IO.utf8
        TIO.hPutStr h contents
        hClose h
        action path)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
