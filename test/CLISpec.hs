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
    it "root"              $ ["--help"]                              `has` ["synapse", "plexus"]
    it "plexus"            $ ["plexus"]                              `has` ["echo", "solar", "health"]
    it "echo"              $ ["plexus", "echo"]                      `has` ["echo", "Echo messages back"]
    it "solar"             $ ["plexus", "solar"]                     `has` ["solar", "Solar system model"]
    it "health"            $ ["plexus", "health"]                    `has` ["health", "Check hub health"]
    it "solar earth"       $ ["plexus", "solar", "earth"]            `has` ["earth", "planet"]
    it "solar earth luna"  $ ["plexus", "solar", "earth", "luna"]    `has` ["luna", "Moon"]

  describe "method help" $ do
    it "echo once"    $ ["plexus", "echo", "once"]    `has` ["once", "--message", "required"]
    it "echo echo"    $ ["plexus", "echo", "echo"]    `has` ["--message", "--count"]
    -- health.check has no required params, so it auto-invokes
    it "health check" $ ["plexus", "health", "check"] `has` ["healthy"]

  describe "invocation" $ do
    it "echo once"     $ call ["plexus", "echo", "once"] (msg "test")       `has` ["test"]
    it "echo count"    $ call ["plexus", "echo", "echo"] (msgN "hi" 2)      `has` ["hi", "2"]
    it "health"        $ callRaw ["plexus", "health", "check"] "{}"         `has` ["healthy"]
    it "solar observe" $ callRaw ["plexus", "solar", "observe"] "{}"        `has` ["sol", "planet_count"]
    it "luna info"     $ callRaw ["plexus", "solar", "earth", "luna", "info"] "{}" `has` ["luna"]

-- ============================================================================
-- Harness
-- ============================================================================

synapse :: FilePath
synapse = "dist-newstyle/build/aarch64-osx/ghc-9.6.7/hub-synapse-0.2.0.0/x/synapse/build/synapse/synapse"

-- | Assert synapse output contains all substrings
has :: [String] -> [Text] -> Expectation
has = checkOutput synapse

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

-- | Build invocation args with --raw (skip templates)
-- Note: --raw must come after "plexus" subcommand
callRaw :: [String] -> String -> [String]
callRaw (backend:rest) params = [backend, "--raw"] ++ rest ++ ["-p", params]
callRaw [] params = ["-p", params]  -- fallback, shouldn't happen

-- | JSON builders
msg :: String -> String
msg m = "{\"message\":\"" <> m <> "\"}"

msgN :: String -> Int -> String
msgN m n = "{\"message\":\"" <> m <> "\",\"count\":" <> show n <> "}"
