{-# LANGUAGE OverloadedStrings #-}

-- | Test that demonstrates Reporter working with real Validator output
--
-- Usage: cabal run validation-report-test --flag build-examples

module Main where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.IO as TIO
import Synapse.Self.Protocol.Validator (validateStreamItem)
import Synapse.Self.Protocol.Reporter
  ( renderViolations
  , renderSummary
  , renderCompact
  , renderJSON
  )

main :: IO ()
main = do
  putStrLn "==================================================================="
  putStrLn "Validation + Reporter Integration Test"
  putStrLn "==================================================================="
  putStrLn ""

  -- Test 1: Invalid stream item (missing type field)
  putStrLn "Test 1: Missing 'type' field"
  putStrLn "-------------------------------------------------------------------"
  let invalidItem1 = object
        [ "metadata" .= object
            [ "provenance" .= (["test-server"] :: [String])
            , "plexus_hash" .= ("1234567890abcdef" :: String)
            , "timestamp" .= (1234567890 :: Int)
            ]
        ]
  violations1 <- validateStreamItem invalidItem1
  TIO.putStrLn $ renderCompact violations1
  putStrLn ""

  -- Test 2: Invalid metadata (bad plexus_hash)
  putStrLn "Test 2: Invalid plexus_hash (too short)"
  putStrLn "-------------------------------------------------------------------"
  let invalidItem2 = object
        [ "type" .= ("data" :: String)
        , "content_type" .= ("test.data" :: String)
        , "content" .= object ["value" .= (42 :: Int)]
        , "metadata" .= object
            [ "provenance" .= (["test-server"] :: [String])
            , "plexus_hash" .= ("abc" :: String)  -- Too short!
            , "timestamp" .= (1234567890 :: Int)
            ]
        ]
  violations2 <- validateStreamItem invalidItem2
  TIO.putStrLn $ renderCompact violations2
  putStrLn ""

  -- Test 3: Valid item (no violations)
  putStrLn "Test 3: Valid stream item"
  putStrLn "-------------------------------------------------------------------"
  let validItem = object
        [ "type" .= ("data" :: String)
        , "content_type" .= ("test.data" :: String)
        , "content" .= object ["value" .= (42 :: Int)]
        , "metadata" .= object
            [ "provenance" .= (["test-server"] :: [String])
            , "plexus_hash" .= ("1234567890abcdef" :: String)
            , "timestamp" .= (1234567890 :: Int)
            ]
        ]
  violations3 <- validateStreamItem validItem
  TIO.putStrLn $ renderSummary violations3
  putStrLn ""

  -- Test 4: Multiple violations in one item
  putStrLn "Test 4: Multiple violations (missing fields + wrong types)"
  putStrLn "-------------------------------------------------------------------"
  let invalidItem4 = object
        [ "type" .= ("data" :: String)
        , "content_type" .= ("" :: String)  -- Empty string
        -- Missing 'content' field
        , "metadata" .= object
            [ "provenance" .= ([] :: [String])  -- Empty array
            , "plexus_hash" .= (123 :: Int)  -- Wrong type (number instead of string)
            , "timestamp" .= (1234567890.5 :: Double)  -- Float instead of integer
            ]
        ]
  violations4 <- validateStreamItem invalidItem4
  TIO.putStrLn $ renderViolations violations4
  putStrLn ""

  -- Summary
  putStrLn "==================================================================="
  putStrLn "Summary Statistics"
  putStrLn "==================================================================="
  let totalViolations = length violations1 + length violations2 +
                       length violations3 + length violations4
  putStrLn $ "Total violations across all tests: " ++ show totalViolations
  putStrLn ""
