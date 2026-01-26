{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for CLI parameter parsing
module Main where

import Data.Aeson (Value(..), object)
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import Synapse.CLI.Parse
import Synapse.IR.Types

main :: IO ()
main = hspec $ do
  describe "groupByPrefix" $ do
    it "groups dotted keys by prefix" $ do
      let input = [("identifier.type", "by_name"), ("identifier.name", "foo"), ("prompt", "hi")]
      let expected = Map.fromList
            [ ("identifier", [("type", "by_name"), ("name", "foo")])
            , ("prompt", [("", "hi")])
            ]
      groupByPrefix input `shouldBe` expected

    it "collects repeated keys for arrays" $ do
      let input = [("tags", "backend"), ("tags", "critical"), ("tags", "urgent")]
      let expected = Map.fromList
            [ ("tags", [("", "backend"), ("", "critical"), ("", "urgent")])
            ]
      groupByPrefix input `shouldBe` expected

  describe "buildParamValue with arrays" $ do
    let emptyIR = IR
          { irVersion = "2.0"
          , irHash = Nothing
          , irTypes = Map.empty
          , irMethods = Map.empty
          , irPlugins = Map.empty
          }

    it "builds string array from repeated values" $ do
      let param = ParamDef
            { pdName = "tags"
            , pdType = RefArray (RefPrimitive "string" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            }
      let kvs = [("", "backend"), ("", "critical"), ("", "urgent")]
      let expected = Right $ Array $ V.fromList [String "backend", String "critical", String "urgent"]
      buildParamValue emptyIR param kvs `shouldBe` expected

    it "builds integer array from repeated values" $ do
      let param = ParamDef
            { pdName = "ids"
            , pdType = RefArray (RefPrimitive "integer" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            }
      let kvs = [("", "1"), ("", "2"), ("", "3")]
      let expected = Right $ Array $ V.fromList [Number 1, Number 2, Number 3]
      buildParamValue emptyIR param kvs `shouldBe` expected

    it "fails when array has no values" $ do
      let param = ParamDef
            { pdName = "tags"
            , pdType = RefArray (RefPrimitive "string" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            }
      let kvs = []
      buildParamValue emptyIR param kvs `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
