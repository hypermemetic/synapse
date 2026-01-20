{-# LANGUAGE OverloadedStrings #-}

-- | Quick test to see TypeRef JSON serialization
module Main where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)

import Synapse.IR.Types

main :: IO ()
main = do
  putStrLn "=== TypeRef JSON Serialization Test ==="
  putStrLn ""

  -- Test 1: Simple QualifiedName with namespace
  putStrLn "1. RefNamed with namespace:"
  let qn1 = QualifiedName { qnNamespace = "cone", qnLocalName = "UUID" }
  let ref1 = RefNamed qn1
  putStrLn "   Haskell: "
  print ref1
  putStrLn "   JSON (compact):"
  BSL.putStrLn (encode ref1)
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref1)
  putStrLn ""

  -- Test 2: Empty namespace
  putStrLn "2. RefNamed with empty namespace:"
  let qn2 = QualifiedName { qnNamespace = "", qnLocalName = "GlobalType" }
  let ref2 = RefNamed qn2
  putStrLn "   Haskell: "
  print ref2
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref2)
  putStrLn ""

  -- Test 3: Nested structure
  putStrLn "3. RefOptional (RefArray (RefNamed)):"
  let qn3 = QualifiedName { qnNamespace = "arbor", qnLocalName = "Node" }
  let ref3 = RefOptional (RefArray (RefNamed qn3))
  putStrLn "   Haskell: "
  print ref3
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref3)
  putStrLn ""

  -- Test 4: qualifiedNameFull function
  putStrLn "4. qualifiedNameFull examples:"
  putStrLn $ "   cone.UUID -> " <> show (qualifiedNameFull qn1)
  putStrLn $ "   GlobalType -> " <> show (qualifiedNameFull qn2)
  putStrLn ""

  -- Test 5: FieldDef with QualifiedName
  putStrLn "5. FieldDef with RefNamed:"
  let fd = FieldDef
        { fdName = "tree_id"
        , fdType = RefNamed (QualifiedName "arbor" "UUID")
        , fdDescription = Just "Tree identifier"
        , fdRequired = True
        , fdDefault = Nothing
        }
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty fd)
