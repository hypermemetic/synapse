{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Hspec
import Data.Aeson (Value(..), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

import Synapse.Self.Protocol.StreamTracker
import Synapse.Self.Protocol.Validator

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "StreamTracker" $ do
    describe "trackMessage" $ do
      it "tracks subscription messages" $ do
        tracker <- newTracker
        let msg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"hello\" } } }"
        violations <- trackMessage tracker msg
        violations `shouldBe` []
        count <- getStreamCount tracker
        count `shouldBe` 1

      it "detects missing StreamDone violation" $ do
        tracker <- newTracker
        let dataMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"hello\" } } }"
        _ <- trackMessage tracker dataMsg
        violations <- checkCompletion tracker
        length violations `shouldBe` 1
        pvSeverity (head violations) `shouldBe` Error

      it "detects duplicate StreamDone violation" $ do
        tracker <- newTracker
        let doneMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"
        _ <- trackMessage tracker doneMsg
        violations <- trackMessage tracker doneMsg
        length violations `shouldBe` 1
        pvSeverity (head violations) `shouldBe` Error

      it "properly completes stream with StreamDone" $ do
        tracker <- newTracker
        let dataMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"hello\" } } }"
        let doneMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"
        _ <- trackMessage tracker dataMsg
        _ <- trackMessage tracker doneMsg
        violations <- checkCompletion tracker
        violations `shouldBe` []

      it "tracks multiple subscriptions independently" $ do
        tracker <- newTracker
        let msg1 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"stream1\" } } }"
        let msg2 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 2, \"result\": { \"type\": \"data\", \"content\": \"stream2\" } } }"
        let done1 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"

        _ <- trackMessage tracker msg1
        _ <- trackMessage tracker msg2
        _ <- trackMessage tracker done1

        count <- getStreamCount tracker
        count `shouldBe` 2

        -- Only subscription 2 should be incomplete
        violations <- checkCompletion tracker
        length violations `shouldBe` 1

      it "detects messages after StreamDone" $ do
        tracker <- newTracker
        let doneMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"
        let dataMsg = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"after done\" } } }"

        _ <- trackMessage tracker doneMsg
        violations <- trackMessage tracker dataMsg

        length violations `shouldBe` 1
        pvSeverity (head violations) `shouldBe` Error

    describe "getAllViolations" $ do
      it "aggregates all violations from all streams" $ do
        tracker <- newTracker
        let msg1 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"data\", \"content\": \"stream1\" } } }"
        let msg2 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 2, \"result\": { \"type\": \"data\", \"content\": \"stream2\" } } }"
        let doneMsg1 = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"
        let doneMsg1Dup = parseJson "{ \"jsonrpc\": \"2.0\", \"method\": \"subscription\", \"params\": { \"subscription\": 1, \"result\": { \"type\": \"done\" } } }"

        _ <- trackMessage tracker msg1
        _ <- trackMessage tracker msg2
        _ <- trackMessage tracker doneMsg1
        _ <- trackMessage tracker doneMsg1Dup  -- Duplicate done

        allViolations <- getAllViolations tracker
        -- Should have: 1 duplicate StreamDone + 1 missing StreamDone for stream 2
        length allViolations `shouldBe` 2

-- Helper to parse JSON strings
parseJson :: String -> Value
parseJson s = fromJust $ decode (BL.pack s)
