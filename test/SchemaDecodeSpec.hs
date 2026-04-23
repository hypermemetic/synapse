{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | REQ-S10 spike: verify Plexus.Schema.Recursive.PluginSchema decodes
-- the new optional psRequest field losslessly, including x-plexus-source
-- extension nodes.
module Main where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Vector as V
import System.Exit (exitFailure, exitSuccess)
import qualified Data.IORef as IORef

import Plexus.Schema.Recursive (PluginSchema(..))

-- ============================================================================
-- Fixtures
-- ============================================================================

fixtureWithRequest :: Value
fixtureWithRequest = [aesonQQ|{
  "namespace": "clients",
  "version": "1.0.0",
  "description": "Client management",
  "hash": "abc123",
  "methods": [],
  "request": {
    "type": "object",
    "properties": {
      "auth_token": {
        "type": "string",
        "description": "JWT from Keycloak",
        "x-plexus-source": { "from": "cookie", "key": "access_token" }
      },
      "origin": {
        "type": "string",
        "x-plexus-source": { "from": "header", "key": "origin" }
      },
      "peer_addr": {
        "type": "string",
        "x-plexus-source": { "from": "derived" }
      }
    },
    "required": ["auth_token"]
  }
}|]

fixtureWithoutRequest :: Value
fixtureWithoutRequest = [aesonQQ|{
  "namespace": "forms",
  "version": "1.0.0",
  "description": "Forms (no auth required)",
  "hash": "def456",
  "methods": []
}|]

fixtureEmptyRequest :: Value
fixtureEmptyRequest = [aesonQQ|{
  "namespace": "edge",
  "version": "1.0.0",
  "description": "Edge case",
  "hash": "ghi789",
  "methods": [],
  "request": {}
}|]

-- ============================================================================
-- Test runner
-- ============================================================================

data TestResult = Pass String | Fail String String

runTest :: String -> Bool -> String -> TestResult
runTest name True  _   = Pass name
runTest name False why = Fail name why

main :: IO ()
main = do
  failures <- IORef.newIORef (0 :: Int)
  let run :: TestResult -> IO ()
      run (Pass name)        = putStrLn $ "  [PASS] " <> name
      run (Fail name reason) = do
        putStrLn $ "  [FAIL] " <> name <> ": " <> reason
        IORef.modifyIORef' failures (+ 1)

  putStrLn "=== REQ-S10: PluginSchema psRequest decoding ==="

  -- Pass condition 1: fixtureWithRequest decodes to psRequest = Just _
  -- and x-plexus-source.from on auth_token is "cookie"
  case fromJSON fixtureWithRequest :: Result PluginSchema of
    Error e -> run (Fail "fixture-with-request decodes" e)
    Success schema -> do
      run $ runTest "fixture-with-request: psRequest = Just _"
              (isJust (psRequest schema))
              "expected Just, got Nothing"
      let cookieFrom = do
            req <- psRequest schema
            obj <- valueToObject req
            props <- KeyMap.lookup "properties" obj
            propsObj <- valueToObject props
            authTok <- KeyMap.lookup "auth_token" propsObj
            atObj <- valueToObject authTok
            src <- KeyMap.lookup "x-plexus-source" atObj
            srcObj <- valueToObject src
            from <- KeyMap.lookup "from" srcObj
            valueToString from
      run $ runTest "fixture-with-request: auth_token x-plexus-source.from == 'cookie'"
              (cookieFrom == Just "\"cookie\"")
              ("got: " <> show cookieFrom)

  -- Pass condition 2: fixtureWithoutRequest decodes to psRequest = Nothing
  case fromJSON fixtureWithoutRequest :: Result PluginSchema of
    Error e -> run (Fail "fixture-without-request decodes" e)
    Success schema ->
      run $ runTest "fixture-without-request: psRequest = Nothing"
              (isNothing (psRequest schema))
              ("expected Nothing, got: " <> show (psRequest schema))

  -- Pass condition 3: fixtureEmptyRequest decodes to psRequest = Just (Object empty)
  case fromJSON fixtureEmptyRequest :: Result PluginSchema of
    Error e -> run (Fail "fixture-empty-request decodes" e)
    Success schema -> do
      let isEmptyObject = case psRequest schema of
            Just (Object km) -> KeyMap.null km
            _                -> False
      run $ runTest "fixture-empty-request: psRequest = Just (Object empty)"
              isEmptyObject
              ("got: " <> show (psRequest schema))

  -- Pass condition 4: lossless roundtrip on all three fixtures
  let roundtripCheck name fixture =
        case fromJSON fixture :: Result PluginSchema of
          Error e -> Fail (name <> " decode for roundtrip") e
          Success original ->
            case decode (encode original) :: Maybe PluginSchema of
              Nothing -> Fail (name <> " roundtrip decode") "encode/decode failed"
              Just decoded ->
                runTest (name <> " roundtrip lossless")
                        (decoded == original)
                        "decoded /= original"

  run $ roundtripCheck "fixture-with-request" fixtureWithRequest
  run $ roundtripCheck "fixture-without-request" fixtureWithoutRequest
  run $ roundtripCheck "fixture-empty-request" fixtureEmptyRequest

  -- Pass condition 5: x-plexus-source survives roundtrip on all three field types
  case fromJSON fixtureWithRequest :: Result PluginSchema of
    Error _ -> run (Fail "x-plexus-source roundtrip" "decode failed")
    Success original -> do
      let reEncoded = encode original
      case decode reEncoded :: Maybe PluginSchema of
        Nothing -> run (Fail "x-plexus-source roundtrip decode" "decode-after-encode failed")
        Just decoded -> do
          let extractFrom propName = do
                req <- psRequest decoded
                obj <- valueToObject req
                props <- KeyMap.lookup "properties" obj >>= valueToObject
                prop <- KeyMap.lookup (Key.fromString propName) props >>= valueToObject
                src <- KeyMap.lookup "x-plexus-source" prop >>= valueToObject
                from <- KeyMap.lookup "from" src
                valueToString from
          run $ runTest "x-plexus-source roundtrip: cookie field"
                  (extractFrom "auth_token" == Just "\"cookie\"") (show $ extractFrom "auth_token")
          run $ runTest "x-plexus-source roundtrip: header field"
                  (extractFrom "origin" == Just "\"header\"") (show $ extractFrom "origin")
          run $ runTest "x-plexus-source roundtrip: derived field"
                  (extractFrom "peer_addr" == Just "\"derived\"") (show $ extractFrom "peer_addr")

  fc <- IORef.readIORef failures
  if fc == 0
    then do
      putStrLn "\nREQ-S10: ALL TESTS PASSED"
      exitSuccess
    else do
      putStrLn $ "\nREQ-S10: " <> show fc <> " test(s) failed"
      exitFailure

-- helpers
valueToObject :: Value -> Maybe Object
valueToObject (Object o) = Just o
valueToObject _          = Nothing

valueToString :: Value -> Maybe String
valueToString (String t) = Just (show t)  -- show wraps in quotes; we compare against quoted strings below
valueToString _          = Nothing
