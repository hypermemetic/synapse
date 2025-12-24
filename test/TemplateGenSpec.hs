{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for schema-driven template generation
module Main where

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Plexus.Template
import Plexus.Template.Schema
import Plexus.Template.Gen

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Schema Parsing" $ do
    it "parses a simple object schema" $ do
      let schema = [aesonQQ|{
        "type": "object",
        "properties": {
          "name": { "type": "string" },
          "count": { "type": "integer" }
        },
        "required": ["name"]
      }|]
      case parseReturnSchema schema of
        Right (RSSingle _ fields) -> do
          length fields `shouldBe` 2
          let names = [n | (n, _, _) <- fields]
          names `shouldContain` ["name"]
          names `shouldContain` ["count"]
        Right other -> expectationFailure $ "Expected RSSingle, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

    it "parses a discriminated union schema" $ do
      let schema = [aesonQQ|{
        "oneOf": [
          {
            "properties": {
              "type": { "const": "message" },
              "content": { "type": "string" }
            },
            "required": ["type", "content"]
          },
          {
            "properties": {
              "type": { "const": "error" },
              "message": { "type": "string" }
            },
            "required": ["type", "message"]
          }
        ]
      }|]
      case parseReturnSchema schema of
        Right (RSUnion disc variants _) -> do
          disc `shouldBe` "type"
          length variants `shouldBe` 2
          let variantNames = map variantName variants
          variantNames `shouldContain` ["message"]
          variantNames `shouldContain` ["error"]
        Right other -> expectationFailure $ "Expected RSUnion, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

    it "parses nullable types" $ do
      let schema = [aesonQQ|{
        "type": "object",
        "properties": {
          "optional_field": { "type": ["string", "null"] }
        }
      }|]
      case parseReturnSchema schema of
        Right (RSSingle _ fields) -> do
          case lookup "optional_field" [(n, s) | (n, s, _) <- fields] of
            Just (FNullable FString) -> pure ()
            Just other -> expectationFailure $ "Expected FNullable FString, got: " ++ show other
            Nothing -> expectationFailure "Field not found"
        Right other -> expectationFailure $ "Expected RSSingle, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

    it "parses array types" $ do
      let schema = [aesonQQ|{
        "type": "object",
        "properties": {
          "items": {
            "type": "array",
            "items": { "type": "string" }
          }
        }
      }|]
      case parseReturnSchema schema of
        Right (RSSingle _ fields) -> do
          case lookup "items" [(n, s) | (n, s, _) <- fields] of
            Just (FArray FString) -> pure ()
            Just other -> expectationFailure $ "Expected FArray FString, got: " ++ show other
            Nothing -> expectationFailure "Field not found"
        Right other -> expectationFailure $ "Expected RSSingle, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

  describe "Template Generation" $ do
    it "generates template for chat_content variant" $ do
      let variant = VariantSchema
            { variantName = "chat_content"
            , variantDesc = Just "Chat content chunk"
            , variantFields = [("content", FString, True), ("cone_id", FString, True)]
            }
      let template = compileFragment $ genVariantTemplate variant
      -- chat_content should just output content directly
      template `shouldBe` "{{{content}}}"

    it "generates template for error variant" $ do
      let variant = VariantSchema
            { variantName = "error"
            , variantDesc = Just "Error message"
            , variantFields = [("message", FString, True)]
            }
      let template = compileFragment $ genVariantTemplate variant
      template `textShouldContain` "{{{message}}}"
      template `textShouldContain` "Error:"

    it "generates template for list variant" $ do
      let variant = VariantSchema
            { variantName = "cone_list"
            , variantDesc = Just "List of cones"
            , variantFields = [("cones", FArray (FObject [("name", FString, True), ("id", FString, True)]), True)]
            }
      let template = compileFragment $ genVariantTemplate variant
      -- Should have a section for cones
      template `textShouldContain` "{{#cones}}"
      template `textShouldContain` "{{/cones}}"

    it "generates templates for full return schema" $ do
      let schema = [aesonQQ|{
        "oneOf": [
          {
            "properties": {
              "type": { "const": "chat_content" },
              "content": { "type": "string" }
            }
          },
          {
            "properties": {
              "type": { "const": "chat_complete" },
              "usage": { "type": "object" }
            }
          }
        ]
      }|]
      case parseReturnSchema schema of
        Right rs -> do
          let ts = genTemplates "cone_chat" rs
          Map.size (tsVariants ts) `shouldBe` 2
          Map.member "chat_content" (tsVariants ts) `shouldBe` True
          Map.member "chat_complete" (tsVariants ts) `shouldBe` True
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

  describe "Rendering" $ do
    it "renders streaming event with correct variant" $ do
      let schema = [aesonQQ|{
        "oneOf": [
          {
            "properties": {
              "type": { "const": "message" },
              "content": { "type": "string" }
            }
          }
        ]
      }|]
      case parseReturnSchema schema of
        Right rs -> do
          let ts = genTemplates "test" rs
          let renderer = mkRenderer ts
          let event = object ["type" .= ("message" :: Text), "content" .= ("Hello!" :: Text)]
          case renderEvent renderer event of
            Rendered text -> T.strip text `textShouldContain` "Hello!"
            other -> expectationFailure $ "Expected Rendered, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err

  describe "Heuristics" $ do
    it "prioritizes content fields highest" $ do
      fieldPriority "content" `shouldBe` 100
      fieldPriority "name" `shouldSatisfy` (< fieldPriority "content")

    it "identifies meta fields correctly" $ do
      isMetaField "type" `shouldBe` True
      isMetaField "created_at" `shouldBe` True
      isMetaField "plexus_hash" `shouldBe` True
      isMetaField "name" `shouldBe` False
      isMetaField "content" `shouldBe` False

    it "formats field names nicely" $ do
      formatFieldName "user_name" `shouldBe` "User Name"
      formatFieldName "created_at" `shouldBe` "Created At"

-- Helper for checking Text contains substring
textShouldContain :: Text -> Text -> Expectation
textShouldContain haystack needle =
  if needle `T.isInfixOf` haystack
    then pure ()
    else expectationFailure $
      "Expected " ++ show haystack ++ " to contain " ++ show needle
