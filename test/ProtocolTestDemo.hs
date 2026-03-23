{-# LANGUAGE OverloadedStrings #-}

-- | Demo program showing how to use the protocol test runner
--
-- Usage:
--   cabal run protocol-test-demo -- <host> <port> <backend>
--
-- Example:
--   cabal run protocol-test-demo -- localhost 3000 bash
--
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Synapse.Self.Protocol.TestRunner (runProtocolTests)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [hostStr, portStr, backendStr] -> do
      let host = T.pack hostStr
      let backend = T.pack backendStr
      case reads portStr of
        [(port, "")] -> runTests host port backend
        _ -> do
          TIO.putStrLn "Error: Invalid port number"
          exitFailure
    _ -> do
      TIO.putStrLn "Usage: protocol-test-demo <host> <port> <backend>"
      TIO.putStrLn "Example: protocol-test-demo localhost 3000 bash"
      exitFailure

runTests :: Text -> Int -> Text -> IO ()
runTests host port backend = do
  TIO.putStrLn $ "Running protocol tests against: " <> host <> ":" <> T.pack (show port) <> " (backend: " <> backend <> ")"
  TIO.putStrLn ""

  result <- runProtocolTests host port backend

  case result of
    Left errorReport -> do
      TIO.putStrLn errorReport
      exitFailure
    Right successReport -> do
      TIO.putStrLn successReport
      exitSuccess
