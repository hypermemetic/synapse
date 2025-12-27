{-# LANGUAGE OverloadedStrings #-}

-- | CLI integration tests
--
-- Pattern: args `has` ["expected", "substrings"]
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess)
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "navigation" $ do
    it "root"              $ []                            `has` ["synapse", "plexus", "methods", "activations"]
    it "echo"              $ ["echo"]                      `has` ["echo", "Echo messages back"]
    it "solar"             $ ["solar"]                     `has` ["solar", "Solar system model"]
    it "health"            $ ["health"]                    `has` ["health", "Check hub health"]
    it "solar earth"       $ ["solar", "earth"]            `has` ["earth", "planet"]
    it "solar earth luna"  $ ["solar", "earth", "luna"]    `has` ["luna", "Moon"]

  describe "method help" $ do
    it "echo once"    $ ["echo", "once"]    `has` ["once", "--message", "required"]
    it "echo echo"    $ ["echo", "echo"]    `has` ["--message", "--count"]
    it "health check" $ ["health", "check"] `has` ["check", "no parameters"]

  describe "invocation" $ do
    it "echo once"     $ call ["echo", "once"] (msg "test")       `has` ["test"]
    it "echo count"    $ call ["echo", "echo"] (msgN "hi" 2)      `has` ["hi", "2"]
    it "health"        $ call ["health", "check"] "{}"            `has` ["healthy"]
    it "solar observe" $ call ["solar", "observe"] "{}"           `has` ["sol", "planet_count"]
    it "luna info"     $ call ["solar", "earth", "luna", "info"] "{}" `has` ["luna"]

  describe "algebra parity" $ do
    it "root"   $ []                               `hasA` ["synapse", "plexus", "methods"]
    it "echo"   $ ["echo"]                         `hasA` ["echo", "Echo messages"]
    it "invoke" $ call ["echo", "once"] (msg "yo") `hasA` ["yo"]

-- ============================================================================
-- Harness
-- ============================================================================

synapse, algebra :: FilePath
synapse = "dist-newstyle/build/aarch64-osx/ghc-9.6.7/synapse-0.1.0.0/x/synapse/build/synapse/synapse"
algebra = "dist-newstyle/build/aarch64-osx/ghc-9.6.7/synapse-0.1.0.0/x/synapse-algebra/build/synapse-algebra/synapse-algebra"

-- | Assert synapse output contains all substrings
has :: [String] -> [Text] -> Expectation
has = checkOutput synapse

-- | Assert algebra output contains all substrings
hasA :: [String] -> [Text] -> Expectation
hasA = checkOutput algebra

-- | Generic output checker
checkOutput :: FilePath -> [String] -> [Text] -> Expectation
checkOutput bin args expected = do
  out <- T.pack <$> readProcess bin args ""
  mapM_ (assertContains out) expected

assertContains :: Text -> Text -> Expectation
assertContains haystack needle =
  T.toLower haystack `shouldSatisfy` T.isInfixOf (T.toLower needle)

-- | Build invocation args
call :: [String] -> String -> [String]
call path params = path ++ ["-p", params]

-- | JSON builders
msg :: String -> String
msg m = "{\"message\":\"" <> m <> "\"}"

msgN :: String -> Int -> String
msgN m n = "{\"message\":\"" <> m <> "\",\"count\":" <> show n <> "}"
