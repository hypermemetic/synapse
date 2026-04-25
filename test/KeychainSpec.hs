{-# LANGUAGE OverloadedStrings #-}

-- | SELF-8 opt-in keychain tests.
--
-- This test-suite shells out to the macOS @security@ CLI and touches the
-- real keychain. It's gated behind the @build-keychain-tests@ cabal flag:
--
-- > cabal test plexus-synapse:keychain-test -f build-keychain-tests
--
-- Default @cabal test@ runs do NOT include this suite.
--
-- Tests use a unique per-run service name (@plexus-selftest-<uuid>@) so
-- they don't collide with real synapse/synapse-cc keychain items. All
-- created items are cleaned up in 'finally' blocks regardless of test
-- outcome.
module Main (main) where

import Control.Exception (finally)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import System.Info (os)

import Synapse.Self
  ( CredentialRef(..)
  , ResolveError(..)
  , deleteFromKeychain
  , isKeychainPlatform
  , resolveRef
  , storeInKeychain
  )
import Synapse.Self.Resolve (registerResolver, emptyRegistry)
import Synapse.Self.Resolve.Keychain (keychainResolver)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Synapse.Self.Resolve.Keychain (SELF-8)" $ do
  if not isKeychainPlatform
    then it ("skipped: keychain backend not implemented on " ++ os) pending
    else keychainMacOSSpec

keychainMacOSSpec :: Spec
keychainMacOSSpec = do
  describe "platform probe" $ do
    it "reports macOS as a keychain platform" $
      isKeychainPlatform `shouldBe` True

  describe "resolveRef via defaultRegistry entry" $ do
    it "returns ResolveNotFound for a keychain item that doesn't exist" $ do
      run <- runId
      let service = "plexus-selftest-" <> run
          reg     = registerResolver "keychain" keychainResolver emptyRegistry
          ref     = CredentialRef $
                      "keychain://" <> service <> "/no-such-account"
      r <- resolveRef reg ref
      r `shouldBe` Left (ResolveNotFound ref)

  describe "store -> resolve -> delete round trip" $ do
    it "stores a value, reads it back, then deletes" $ do
      run <- runId
      let service = "plexus-selftest-" <> run
          account = "roundtrip/access_token"
          value   = "eyJhbGciOiJIUzI1NiJ9.fake"
          ref     = CredentialRef $
                      "keychain://" <> service <> "/" <> account
          reg     = registerResolver "keychain" keychainResolver emptyRegistry
      (do
        rs <- storeInKeychain service account value
        rs `shouldBe` Right ()

        rr <- resolveRef reg ref
        rr `shouldBe` Right value

        rd <- deleteFromKeychain service account
        rd `shouldBe` Right ()

        rr2 <- resolveRef reg ref
        rr2 `shouldBe` Left (ResolveNotFound ref)
        )
        `finally`
          -- Belt-and-suspenders cleanup (no-op if delete already ran).
          deleteFromKeychain service account

  describe "update-in-place (-U)" $ do
    it "overwrites an existing item when stored again" $ do
      run <- runId
      let service = "plexus-selftest-" <> run
          account = "update/access_token"
          v1      = "first"
          v2      = "second"
          ref     = CredentialRef $
                      "keychain://" <> service <> "/" <> account
          reg     = registerResolver "keychain" keychainResolver emptyRegistry
      (do
        _ <- storeInKeychain service account v1
        _ <- storeInKeychain service account v2
        r <- resolveRef reg ref
        r `shouldBe` Right v2
        )
        `finally`
          deleteFromKeychain service account

  describe "delete idempotency" $ do
    it "deleting a non-existent item returns Right ()" $ do
      run <- runId
      let service = "plexus-selftest-" <> run
          account = "never-created"
      r <- deleteFromKeychain service account
      r `shouldBe` Right ()

  describe "URI shape errors" $ do
    let reg = registerResolver "keychain" keychainResolver emptyRegistry

    it "rejects opaque keychain:foo" $ do
      r <- resolveRef reg (CredentialRef "keychain:foo")
      case r of
        Left (ResolveBackendError _ msg) ->
          ("must be hierarchical" `T.isInfixOf` msg) `shouldBe` True
        other -> expectationFailure ("expected ResolveBackendError, got "
                                     <> show other)

    it "rejects empty service" $ do
      r <- resolveRef reg (CredentialRef "keychain:///account")
      case r of
        Left (ResolveParseError msg) ->
          ("service missing" `T.isInfixOf` msg) `shouldBe` True
        other -> expectationFailure ("expected ResolveParseError, got "
                                     <> show other)

    it "rejects empty account" $ do
      r <- resolveRef reg (CredentialRef "keychain://svc")
      case r of
        Left (ResolveParseError msg) ->
          ("account missing" `T.isInfixOf` msg) `shouldBe` True
        other -> expectationFailure ("expected ResolveParseError, got "
                                     <> show other)

-- | Short stable id for each test's service name. Using a UUID fragment
-- keeps tests isolated across parallel runs and across dev/CI machines.
runId :: IO T.Text
runId = do
  u <- nextRandom
  pure $ T.take 8 (toText u)
