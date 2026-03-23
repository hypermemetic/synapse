{-# LANGUAGE OverloadedStrings #-}

-- | Protocol-level debugging tools for synapse
--
-- Use: synapse _self debug <host> <port> <backend>
--
-- This module provides tools to debug WebSocket connection issues,
-- protocol mismatches, and transport-level problems.
module Synapse.Self.Debugger
  ( debugConnection
  ) where

import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSockets as WS
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

-- | Debug connection to a Plexus backend
--
-- Tests:
-- 1. TCP connection
-- 2. HTTP upgrade request
-- 3. WebSocket handshake
-- 4. Protocol version compatibility
debugConnection :: String -> Int -> Text -> IO ()
debugConnection host port backend = do
  putStrLn $ "Debugging connection to " <> host <> ":" <> show port
  putStrLn $ "Backend: " <> T.unpack backend
  putStrLn ""

  -- Step 1: TCP connection
  putStr "[1/4] Testing TCP connection... "
  hFlush stdout
  tcpResult <- timeout 3000000 $ testTCPConnection host port
  case tcpResult of
    Nothing -> do
      putStrLn "TIMEOUT (3s)"
      putStrLn "      TCP connection timed out. Server may be down or unreachable."
      return ()
    Just (Left err) -> do
      putStrLn $ "FAILED: " <> show err
      putStrLn "      Cannot establish TCP connection. Check if server is running."
      return ()
    Just (Right ()) -> do
      putStrLn "OK"

      -- Step 2: HTTP check
      putStr "[2/4] Testing HTTP endpoint... "
      hFlush stdout
      httpResult <- timeout 3000000 $ testHTTPEndpoint host port
      case httpResult of
        Nothing -> putStrLn "TIMEOUT (3s)"
        Just (Left err) -> putStrLn $ "FAILED: " <> show err
        Just (Right response) -> do
          putStrLn "OK"
          putStrLn $ "      " <> T.unpack response

      -- Step 3: WebSocket handshake
      putStr "[3/4] Testing WebSocket upgrade... "
      hFlush stdout
      wsResult <- timeout 5000000 $ testWebSocketHandshake host port
      case wsResult of
        Nothing -> do
          putStrLn "TIMEOUT (5s)"
          putStrLn ""
          putStrLn "ROOT CAUSE: WebSocket handshake hangs"
          putStrLn "  The server accepts TCP connections but doesn't complete"
          putStrLn "  the WebSocket upgrade. This indicates:"
          putStrLn "  - Server is not a WebSocket server on this port"
          putStrLn "  - Protocol version mismatch"
          putStrLn "  - Server is waiting for different handshake format"
        Just (Left err) -> do
          putStrLn $ "FAILED: " <> show err
          putStrLn ""
          putStrLn "WebSocket handshake rejected by server"
        Just (Right ()) -> do
          putStrLn "OK"

          -- Step 4: Protocol test
          putStr "[4/4] Testing Plexus RPC protocol... "
          hFlush stdout
          rpcResult <- timeout 5000000 $ testPlexusRPC host port backend
          case rpcResult of
            Nothing -> do
              putStrLn "TIMEOUT (5s)"
              putStrLn ""
              putStrLn "ROOT CAUSE: Plexus RPC protocol timeout"
              putStrLn "  The server accepts WebSocket connections but:"
              putStrLn "  - Does not send required StreamDone messages"
              putStrLn "  - Does not respond to _info requests correctly"
              putStrLn "  - May not be a valid Plexus RPC server"
            Just (Left err) -> putStrLn $ "FAILED: " <> show err
            Just (Right ()) -> putStrLn "OK"

-- | Test TCP connection
testTCPConnection :: String -> Int -> IO (Either SomeException ())
testTCPConnection host port = try $ do
  addr <- NS.getAddrInfo
    (Just NS.defaultHints { NS.addrSocketType = NS.Stream })
    (Just host)
    (Just $ show port)
  case addr of
    [] -> error "No address info"
    (serverAddr:_) -> do
      sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress serverAddr)
      NS.close sock

-- | Test HTTP endpoint
testHTTPEndpoint :: String -> Int -> IO (Either SomeException Text)
testHTTPEndpoint host port = try $ do
  addr <- NS.getAddrInfo
    (Just NS.defaultHints { NS.addrSocketType = NS.Stream })
    (Just host)
    (Just $ show port)
  case addr of
    [] -> error "No address info"
    (serverAddr:_) -> do
      sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress serverAddr)
      let request = "GET / HTTP/1.1\r\nHost: " <> BS.pack (host <> ":" <> show port) <> "\r\n\r\n"
      NSB.sendAll sock request
      response <- NSB.recv sock 1024
      NS.close sock
      return $ T.pack $ BS.unpack $ BS.take 100 response

-- | Test WebSocket handshake
testWebSocketHandshake :: String -> Int -> IO (Either SomeException ())
testWebSocketHandshake host port = try $
  WS.runClient host port "/" $ \_conn -> return ()

-- | Test Plexus RPC protocol
testPlexusRPC :: String -> Int -> Text -> IO (Either SomeException ())
testPlexusRPC host port backend = try $
  WS.runClient host port "/" $ \conn -> do
    -- Send _info request
    let request = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"" <> T.unpack backend <> "._info\",\"params\":{}}"
    WS.sendTextData conn (T.pack request :: Text)

    -- Wait for response (simplified - real implementation would parse)
    _response <- WS.receiveData conn :: IO Text
    return ()
