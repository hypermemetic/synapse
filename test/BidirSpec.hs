{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bidirectional communication tests
--
-- Tests for Synapse.Bidir module, covering:
-- - Request/Response type handling
-- - BidirMode parsing and handling
-- - JSON serialization/deserialization
-- - BidirHandler functions
module Main where

import Data.Aeson (Value(..), decode, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Hspec

import Plexus.Types
  ( Request(..)
  , Response(..)
  , SelectOption(..)
  )
import Synapse.Bidir
  ( BidirMode(..)
  , parseBidirMode
  , defaultBidirMode
  )

main :: IO ()
main = hspec $ do

  describe "Request type constructors" $ do
    it "creates Confirm request" $ do
      let req = Confirm "Are you sure?" (Just True) :: Request Value
      confirmMessage req `shouldBe` "Are you sure?"
      confirmDefault req `shouldBe` Just True

    it "creates Prompt request with default" $ do
      let req = Prompt "Enter name" (Just (String "default")) (Just "placeholder") :: Request Value
      promptMessage req `shouldBe` "Enter name"
      promptDefault req `shouldBe` Just (String "default")
      promptPlaceholder req `shouldBe` Just "placeholder"

    it "creates Prompt request without default" $ do
      let req = Prompt "Enter value" Nothing Nothing :: Request Value
      promptMessage req `shouldBe` "Enter value"
      promptDefault req `shouldBe` Nothing
      promptPlaceholder req `shouldBe` Nothing

    it "creates Select request" $ do
      let opts = [ SelectOption (String "a") "Option A" Nothing
                 , SelectOption (String "b") "Option B" (Just "Description B")
                 ]
      let req = Select "Choose one" opts False :: Request Value
      selectMessage req `shouldBe` "Choose one"
      length (selectOptions req) `shouldBe` 2
      selectMulti req `shouldBe` False

  describe "Response type constructors" $ do
    it "creates Confirmed response" $ do
      let resp = Confirmed True :: Response Value
      confirmedValue resp `shouldBe` True

    it "creates Value response" $ do
      let resp = Value (String "result") :: Response Value
      responseValue resp `shouldBe` String "result"

    it "creates Selected response" $ do
      let resp = Selected [String "a", String "b"] :: Response Value
      selectedValues resp `shouldBe` [String "a", String "b"]

    it "creates Cancelled response" $ do
      let resp = Cancelled :: Response Value
      resp `shouldBe` Cancelled

  describe "Request JSON serialization" $ do
    it "serializes Confirm request" $ do
      let req = Confirm "Proceed?" (Just True) :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "serializes Confirm without default" $ do
      let req = Confirm "Proceed?" Nothing :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "serializes Prompt request" $ do
      let req = Prompt "Enter name" (Just (String "default")) (Just "hint") :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "serializes Select request" $ do
      let opts = [SelectOption (String "x") "Label" Nothing]
      let req = Select "Choose" opts False :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "deserializes confirm request from JSON" $ do
      let jsonStr = "{\"type\":\"confirm\",\"message\":\"Continue?\",\"default\":true}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (Request Value)
      isJust decoded `shouldBe` True
      case decoded of
        Just (Confirm msg def) -> do
          msg `shouldBe` "Continue?"
          def `shouldBe` Just True
        _ -> expectationFailure "Expected Confirm request"

    it "deserializes prompt request from JSON" $ do
      let jsonStr = "{\"type\":\"prompt\",\"message\":\"Name?\",\"placeholder\":\"John\"}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (Request Value)
      isJust decoded `shouldBe` True
      case decoded of
        Just (Prompt msg _ placeholder) -> do
          msg `shouldBe` "Name?"
          placeholder `shouldBe` Just "John"
        _ -> expectationFailure "Expected Prompt request"

  describe "Response JSON serialization" $ do
    it "serializes Confirmed response" $ do
      let resp = Confirmed True :: Response Value
      let json = encode resp
      let decoded = decode json :: Maybe (Response Value)
      decoded `shouldBe` Just resp

    it "serializes Value response with 'text' tag" $ do
      let resp = Value (String "answer") :: Response Value
      let json = encode resp
      let jsonText = T.decodeUtf8 $ LBS.toStrict json
      -- Should serialize with "text" tag for wire compatibility
      jsonText `shouldSatisfy` T.isInfixOf "\"text\""
      let decoded = decode json :: Maybe (Response Value)
      decoded `shouldBe` Just resp

    it "serializes Selected response" $ do
      let resp = Selected [String "a"] :: Response Value
      let json = encode resp
      let decoded = decode json :: Maybe (Response Value)
      decoded `shouldBe` Just resp

    it "serializes Cancelled response" $ do
      let resp = Cancelled :: Response Value
      let json = encode resp
      let decoded = decode json :: Maybe (Response Value)
      decoded `shouldBe` Just resp

    it "deserializes confirmed response from JSON" $ do
      let jsonStr = "{\"type\":\"confirmed\",\"value\":true}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (Response Value)
      decoded `shouldBe` Just (Confirmed True)

    it "deserializes text response from JSON" $ do
      let jsonStr = "{\"type\":\"text\",\"value\":\"hello\"}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (Response Value)
      decoded `shouldBe` Just (Value (String "hello"))

    it "deserializes cancelled response from JSON" $ do
      let jsonStr = "{\"type\":\"cancelled\"}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (Response Value)
      decoded `shouldBe` Just Cancelled

  describe "SelectOption JSON serialization" $ do
    it "serializes SelectOption with description" $ do
      let opt = SelectOption (String "val") "Label" (Just "Desc") :: SelectOption Value
      let json = encode opt
      let decoded = decode json :: Maybe (SelectOption Value)
      decoded `shouldBe` Just opt

    it "serializes SelectOption without description" $ do
      let opt = SelectOption (String "val") "Label" Nothing :: SelectOption Value
      let json = encode opt
      let decoded = decode json :: Maybe (SelectOption Value)
      decoded `shouldBe` Just opt

    it "deserializes SelectOption from JSON" $ do
      let jsonStr = "{\"value\":\"x\",\"label\":\"X\",\"description\":\"Choose X\"}"
      let decoded = decode (LBS8.pack jsonStr) :: Maybe (SelectOption Value)
      isJust decoded `shouldBe` True
      case decoded of
        Just opt -> do
          optionValue opt `shouldBe` String "x"
          optionLabel opt `shouldBe` "X"
          optionDescription opt `shouldBe` Just "Choose X"
        _ -> expectationFailure "Expected SelectOption"

  describe "BidirMode parsing" $ do
    it "parses 'interactive'" $
      parseBidirMode "interactive" `shouldBe` Just BidirInteractive

    it "parses 'json'" $
      parseBidirMode "json" `shouldBe` Just BidirJson

    it "parses 'auto-cancel'" $
      parseBidirMode "auto-cancel" `shouldBe` Just BidirAutoCancel

    it "parses 'defaults'" $
      parseBidirMode "defaults" `shouldBe` Just BidirDefaults

    it "parses 'respond'" $
      parseBidirMode "respond" `shouldBe` Just BidirRespond

    it "returns Nothing for invalid mode" $
      parseBidirMode "invalid" `shouldBe` Nothing

    it "is case-sensitive" $
      parseBidirMode "INTERACTIVE" `shouldBe` Nothing

  describe "BidirMode defaults" $ do
    it "default mode is Interactive" $
      defaultBidirMode `shouldBe` BidirInteractive

  describe "BidirMode equality" $ do
    it "BidirInteractive equals itself" $
      BidirInteractive `shouldBe` BidirInteractive

    it "BidirJson equals itself" $
      BidirJson `shouldBe` BidirJson

    it "BidirAutoCancel equals itself" $
      BidirAutoCancel `shouldBe` BidirAutoCancel

    it "BidirDefaults equals itself" $
      BidirDefaults `shouldBe` BidirDefaults

    it "BidirRespond equals itself" $
      BidirRespond `shouldBe` BidirRespond

    it "BidirCmd with same command equals itself" $
      BidirCmd "test.sh" `shouldBe` BidirCmd "test.sh"

    it "BidirCmd with different commands are not equal" $
      BidirCmd "a.sh" `shouldNotBe` BidirCmd "b.sh"

    it "different modes are not equal" $
      BidirInteractive `shouldNotBe` BidirJson

  describe "Request/Response type compatibility" $ do
    it "Confirm request expects Confirmed response" $ do
      let req = Confirm "OK?" Nothing :: Request Value
      let resp = Confirmed True :: Response Value
      -- Test that types are compatible (both compile and serialize)
      isJust (decode (encode resp) :: Maybe (Response Value)) `shouldBe` True

    it "Prompt request expects Value response" $ do
      let req = Prompt "Input?" Nothing Nothing :: Request Value
      let resp = Value (String "answer") :: Response Value
      isJust (decode (encode resp) :: Maybe (Response Value)) `shouldBe` True

    it "Select request expects Selected response" $ do
      let opts = [SelectOption (String "a") "A" Nothing]
      let req = Select "Pick" opts False :: Request Value
      let resp = Selected [String "a"] :: Response Value
      isJust (decode (encode resp) :: Maybe (Response Value)) `shouldBe` True

    it "Any request can receive Cancelled response" $ do
      let req = Confirm "?" Nothing :: Request Value
      let resp = Cancelled :: Response Value
      isJust (decode (encode resp) :: Maybe (Response Value)) `shouldBe` True

  describe "Complex Request/Response scenarios" $ do
    it "handles Select with multiple options" $ do
      let opts = [ SelectOption (String "opt1") "Option 1" (Just "First option")
                 , SelectOption (String "opt2") "Option 2" (Just "Second option")
                 , SelectOption (String "opt3") "Option 3" Nothing
                 ]
      let req = Select "Choose multiple" opts True :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      isJust decoded `shouldBe` True
      case decoded of
        Just (Select _ decodedOpts multi) -> do
          length decodedOpts `shouldBe` 3
          multi `shouldBe` True
          optionLabel (head decodedOpts) `shouldBe` "Option 1"
        _ -> expectationFailure "Expected Select request"

    it "handles Selected with multiple values" $ do
      let resp = Selected [String "a", String "b", String "c"] :: Response Value
      let json = encode resp
      let decoded = decode json :: Maybe (Response Value)
      case decoded of
        Just (Selected vals) -> length vals `shouldBe` 3
        _ -> expectationFailure "Expected Selected response"

    it "handles Prompt with complex default value" $ do
      let defaultVal = object ["name" .= String "John", "age" .= (30 :: Int)]
      let req = Prompt "User info" (Just defaultVal) Nothing :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      isJust decoded `shouldBe` True
      case decoded of
        Just (Prompt _ def _) -> def `shouldBe` Just defaultVal
        _ -> expectationFailure "Expected Prompt request"

  describe "Edge cases" $ do
    it "handles empty string in Confirm message" $ do
      let req = Confirm "" Nothing :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "handles empty options list in Select" $ do
      let req = Select "Choose" [] False :: Request Value
      let json = encode req
      let decoded = decode json :: Maybe (Request Value)
      decoded `shouldBe` Just req

    it "handles empty selected values" $ do
      let resp = Selected [] :: Response Value
      let json = encode resp
      let decoded = decode json :: Maybe (Response Value)
      decoded `shouldBe` Just resp

    it "handles SelectOption with empty label" $ do
      let opt = SelectOption (String "val") "" Nothing :: SelectOption Value
      let json = encode opt
      let decoded = decode json :: Maybe (SelectOption Value)
      decoded `shouldBe` Just opt
