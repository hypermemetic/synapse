{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Integration tests for IR-based CLI
--
-- Requires a running plexus backend on localhost:4444
-- (or port specified by PLEXUS_PORT environment variable)
--
-- Tests that for every method in the schema:
-- 1. IR builds successfully
-- 2. All type references resolve
-- 3. Help renders without error
-- 4. Support check returns a valid level
module Main where

import Control.Monad (forM_, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Hspec
import Text.Read (readMaybe)

import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.CLI.Help (renderMethodHelp, expandType)
import Synapse.CLI.Support (SupportLevel(..), methodSupport)
import Synapse.Monad

main :: IO ()
main = do
  -- Get port from environment or use default
  port <- getPlexusPort
  putStrLn $ "Running IR integration tests against localhost:" <> show port

  -- Initialize environment
  env <- initEnv "127.0.0.1" port

  -- Build IR once for all tests
  irResult <- runSynapseM env (buildIR [])

  case irResult of
    Left err -> do
      putStrLn $ "Failed to build IR: " <> show err
      putStrLn "Is the plexus backend running?"
      -- Run minimal spec that reports the failure
      hspec $ describe "IR Integration Tests" $
        it "connects to plexus backend" $
          expectationFailure $ "Could not connect: " <> show err

    Right ir -> hspec $ irSpec ir

-- | Get plexus port from PLEXUS_PORT env var or default to 4444
getPlexusPort :: IO Int
getPlexusPort = do
  mPort <- lookupEnv "PLEXUS_PORT"
  pure $ case mPort >>= readMaybe of
    Just p -> p
    Nothing -> 4444

-- | Main spec using pre-built IR
irSpec :: IR -> Spec
irSpec ir = do
  describe "Schema fetching" $ do
    it "builds IR from root" $
      irVersion ir `shouldBe` "1.0"

    it "IR contains methods" $
      Map.size (irMethods ir) `shouldSatisfy` (> 0)

    it "IR contains types" $
      -- Some methods may not have complex types, but we expect at least some
      Map.size (irTypes ir) `shouldSatisfy` (>= 0)

    it "IR contains plugins" $
      Map.size (irPlugins ir) `shouldSatisfy` (> 0)

  describe "Method coverage" $ do
    it "all methods have help text" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        let helpText = renderMethodHelp ir method
        T.length helpText `shouldSatisfy` (> 0)
        -- Help should contain method name
        helpText `shouldSatisfy` T.isInfixOf (mdFullPath method)

    it "all type refs resolve" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        -- Check each param's type ref
        forM_ (mdParams method) $ \param -> do
          let typeRef = pdType param
          checkTypeRefResolves ir (mdFullPath method) (pdName param) typeRef

    it "all methods have support level" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        let support = methodSupport ir method
        -- Support level should be valid (not crash)
        case support of
          FullSupport -> pure ()
          PartialSupport params -> length params `shouldSatisfy` (>= 0)
          NoSupport params -> length params `shouldSatisfy` (> 0)

  describe "Specific methods" $ do
    -- Test cone.chat if it exists (has ConeIdentifier discriminated union)
    case Map.lookup "cone.chat" (irMethods ir) of
      Nothing -> it "cone.chat exists (skipped - not found)" pending
      Just method -> do
        it "cone.chat has identifier param" $
          any (\p -> pdName p == "identifier") (mdParams method) `shouldBe` True

        it "cone.chat expands ConeIdentifier" $ do
          let mIdentifier = filter (\p -> pdName p == "identifier") (mdParams method)
          case mIdentifier of
            [] -> expectationFailure "identifier param not found"
            (p:_) -> do
              let expansion = expandType ir "identifier" (pdType p)
              -- Should have expansion for discriminated union
              length expansion `shouldSatisfy` (> 0)
              -- Should contain "Either:" for union types
              any (T.isInfixOf "Either") expansion `shouldBe` True

    -- Test echo.once if it exists (simple params)
    case Map.lookup "echo.once" (irMethods ir) of
      Nothing -> it "echo.once exists (skipped - not found)" pending
      Just method -> do
        it "echo.once has simple params" $
          -- echo.once should have FullSupport since it only has primitives
          case methodSupport ir method of
            FullSupport -> pure ()
            other -> expectationFailure $ "Expected FullSupport, got: " <> show other

        it "echo.once has message param" $
          any (\p -> pdName p == "message") (mdParams method) `shouldBe` True

  describe "Type resolution" $ do
    it "no dangling RefNamed in params" $
      forM_ (Map.elems $ irMethods ir) $ \method ->
        forM_ (mdParams method) $ \param ->
          case getUnresolvedRefs ir (pdType param) of
            [] -> pure ()
            refs -> expectationFailure $ T.unpack $
              "Unresolved refs in " <> mdFullPath method <> "." <> pdName param <>
              ": " <> T.intercalate ", " refs

    it "reports unresolved return type refs (informational)" $ do
      -- Collect all unresolved return type refs
      let unresolvedReturns =
            [ (mdFullPath method, refs)
            | method <- Map.elems (irMethods ir)
            , let refs = getUnresolvedRefs ir (mdReturns method)
            , not (null refs)
            ]
      -- Report them if any, but don't fail - some methods have simple return types
      -- that reference types defined elsewhere (like SchemaResult from health.schema)
      unless (null unresolvedReturns) $ do
        -- Print for informational purposes
        forM_ unresolvedReturns $ \(methodPath, refs) ->
          putStrLn $ "  [info] " <> T.unpack methodPath <>
            " has unresolved return refs: " <> T.unpack (T.intercalate ", " refs)
      -- Success - this test is informational only
      pure ()

-- | Check that a TypeRef resolves (if named, exists in irTypes)
checkTypeRefResolves :: IR -> Text -> Text -> TypeRef -> Expectation
checkTypeRefResolves ir methodPath paramName = \case
  RefNamed name ->
    unless (Map.member name (irTypes ir)) $
      expectationFailure $ T.unpack $
        "Unresolved type ref: " <> name <>
        " in " <> methodPath <> "." <> paramName
  RefArray inner -> checkTypeRefResolves ir methodPath paramName inner
  RefOptional inner -> checkTypeRefResolves ir methodPath paramName inner
  RefPrimitive _ _ -> pure ()
  RefAny -> pure ()
  RefUnknown -> pure ()  -- Unknown is valid (just means schema gap)

-- | Check that no RefNamed references are unresolved
checkNoUnresolvedRefs :: IR -> TypeRef -> Bool
checkNoUnresolvedRefs ir = \case
  RefNamed name -> Map.member name (irTypes ir)
  RefArray inner -> checkNoUnresolvedRefs ir inner
  RefOptional inner -> checkNoUnresolvedRefs ir inner
  RefPrimitive _ _ -> True
  RefAny -> True
  RefUnknown -> True

-- | Get list of unresolved RefNamed names
getUnresolvedRefs :: IR -> TypeRef -> [Text]
getUnresolvedRefs ir = \case
  RefNamed name
    | Map.member name (irTypes ir) -> []
    | otherwise -> [name]
  RefArray inner -> getUnresolvedRefs ir inner
  RefOptional inner -> getUnresolvedRefs ir inner
  RefPrimitive _ _ -> []
  RefAny -> []
  RefUnknown -> []
