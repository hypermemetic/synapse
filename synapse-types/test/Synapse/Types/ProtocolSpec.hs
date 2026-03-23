{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Synapse.Types.ProtocolSpec where

import Data.Either (isLeft, isRight)
import Test.Hspec
import Synapse.Types.Protocol

spec :: Spec
spec = do
  describe "StreamMetadata" $ do
    it "accepts valid metadata" $ do
      let result = mkStreamMetadata "substrate" "abc123def4567890" 1735052400
      result `shouldSatisfy` isRight

    it "rejects invalid hash" $ do
      let result = mkStreamMetadata "substrate" "short" 1735052400
      result `shouldSatisfy` isLeft

    it "rejects zero timestamp" $ do
      let result = mkStreamMetadata "substrate" "abc123def4567890" 0
      result `shouldSatisfy` isLeft

  describe "Percentage" $ do
    it "accepts 0" $ do
      let result = mkPercentage 0
      result `shouldSatisfy` isRight

    it "accepts 100" $ do
      let result = mkPercentage 100
      result `shouldSatisfy` isRight

    it "accepts 50" $ do
      let result = mkPercentage 50
      result `shouldSatisfy` isRight

    it "rejects -1" $ do
      let result = mkPercentage (-1)
      result `shouldSatisfy` isLeft

    it "rejects 101" $ do
      let result = mkPercentage 101
      result `shouldSatisfy` isLeft

  describe "jsonRpcVersion" $ do
    it "is 2.0" $ do
      jsonRpcVersion `shouldBe` "2.0"
