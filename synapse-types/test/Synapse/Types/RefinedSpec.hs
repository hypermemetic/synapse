{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Synapse.Types.RefinedSpec where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Test.QuickCheck
import Synapse.Types.Refined

spec :: Spec
spec = do
  describe "PlexusHash" $ do
    it "accepts valid 16-char hex hash" $ do
      let result = mkPlexusHash "abc123def4567890"
      result `shouldSatisfy` isRight

    it "rejects short hash" $ do
      let result = mkPlexusHash "short"
      result `shouldSatisfy` isLeft

    it "rejects long hash" $ do
      let result = mkPlexusHash "toolongforaplexushash"
      result `shouldSatisfy` isLeft

    it "rejects uppercase hex" $ do
      let result = mkPlexusHash "ABC123DEF4567890"
      result `shouldSatisfy` isLeft

    it "rejects non-hex characters" $ do
      let result = mkPlexusHash "xyz123def4567890"
      result `shouldSatisfy` isLeft

  describe "Timestamp" $ do
    it "accepts positive timestamp" $ do
      let result = mkTimestamp 1735052400
      result `shouldSatisfy` isRight

    it "rejects zero" $ do
      let result = mkTimestamp 0
      result `shouldSatisfy` isLeft

    it "rejects negative timestamp" $ do
      let result = mkTimestamp (-42)
      result `shouldSatisfy` isLeft

    it "round-trips through unTimestamp" $ property $
      \(Positive n) ->
        let Right ts = mkTimestamp n
        in unTimestamp ts == n

  describe "SnakeCaseField" $ do
    it "accepts valid snake_case" $ do
      let result = mkSnakeCaseField "plexus_hash"
      result `shouldSatisfy` isRight

    it "accepts field with numbers" $ do
      let result = mkSnakeCaseField "field_123"
      result `shouldSatisfy` isRight

    it "rejects camelCase" $ do
      let result = mkSnakeCaseField "plexusHash"
      result `shouldSatisfy` isLeft

    it "rejects starting with underscore" $ do
      let result = mkSnakeCaseField "_field"
      result `shouldSatisfy` isLeft

    it "rejects uppercase letters" $ do
      let result = mkSnakeCaseField "Field_Name"
      result `shouldSatisfy` isLeft

    it "rejects spaces" $ do
      let result = mkSnakeCaseField "field name"
      result `shouldSatisfy` isLeft

  describe "Namespace" $ do
    it "accepts valid namespace" $ do
      let result = mkNamespace "cone"
      result `shouldSatisfy` isRight

    it "accepts namespace with underscores" $ do
      let result = mkNamespace "cone_chat"
      result `shouldSatisfy` isRight

    it "rejects empty namespace" $ do
      let result = mkNamespace ""
      result `shouldSatisfy` isLeft

    it "rejects uppercase" $ do
      let result = mkNamespace "Cone"
      result `shouldSatisfy` isLeft

  describe "Port" $ do
    it "accepts valid port" $ do
      let result = mkPort 8080
      result `shouldSatisfy` isRight

    it "accepts port 1" $ do
      let result = mkPort 1
      result `shouldSatisfy` isRight

    it "accepts port 65535" $ do
      let result = mkPort 65535
      result `shouldSatisfy` isRight

    it "rejects port 0" $ do
      let result = mkPort 0
      result `shouldSatisfy` isLeft

    it "rejects port > 65535" $ do
      let result = mkPort 70000
      result `shouldSatisfy` isLeft

    it "rejects negative port" $ do
      let result = mkPort (-1)
      result `shouldSatisfy` isLeft

  describe "SubscriptionId" $ do
    it "accepts zero" $ do
      let result = mkSubscriptionId 0
      result `shouldSatisfy` isRight

    it "accepts positive" $ do
      let result = mkSubscriptionId 42
      result `shouldSatisfy` isRight

    it "rejects negative" $ do
      let result = mkSubscriptionId (-1)
      result `shouldSatisfy` isLeft

  describe "RequestId" $ do
    it "accepts positive" $ do
      let result = mkRequestId 1
      result `shouldSatisfy` isRight

    it "rejects zero" $ do
      let result = mkRequestId 0
      result `shouldSatisfy` isLeft

    it "rejects negative" $ do
      let result = mkRequestId (-1)
      result `shouldSatisfy` isLeft
