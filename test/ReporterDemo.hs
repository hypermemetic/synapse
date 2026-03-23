{-# LANGUAGE OverloadedStrings #-}

-- | Simple demo of the Reporter module
--
-- Usage: cabal run reporter-demo
--
-- This demonstrates all four output formats:
-- - Compiler-style (renderViolations)
-- - Summary (renderSummary)
-- - Compact (renderCompact)
-- - JSON (renderJSON)

module Main where

import qualified Data.Text.IO as TIO
import Synapse.Self.Protocol.Validator
  ( ProtocolViolation(..)
  , Severity(..)
  , Location(..)
  )
import Synapse.Self.Protocol.Reporter
  ( renderViolations
  , renderSummary
  , renderCompact
  , renderJSON
  )

main :: IO ()
main = do
  putStrLn "==================================================================="
  putStrLn "Protocol Violation Reporter Demo"
  putStrLn "==================================================================="
  putStrLn ""

  -- Create sample violations
  let violations =
        [ ProtocolViolation
            { pvMessage = "Missing StreamDone message"
            , pvSeverity = Error
            , pvLocation = InStream "echo.request" 5
            , pvExpected = Just "StreamDone message to complete stream"
            , pvActual = Just "Stream ended without StreamDone"
            , pvFix = Just "Ensure server sends StreamDone after all data"
            }
        , ProtocolViolation
            { pvMessage = "Missing 'plexus_hash' field in metadata"
            , pvSeverity = Error
            , pvLocation = InMessage (Just 42) 3 (Just "metadata")
            , pvExpected = Just "16-character lowercase hex string"
            , pvActual = Just "missing"
            , pvFix = Just "Add plexus_hash field with schema version hash"
            }
        , ProtocolViolation
            { pvMessage = "Protocol field 'contentType' uses camelCase (should be snake_case)"
            , pvSeverity = Warning
            , pvLocation = InMessage (Just 42) 2 (Just "contentType")
            , pvExpected = Just "content_type"
            , pvActual = Just "contentType"
            , pvFix = Just "Rename to: content_type"
            }
        , ProtocolViolation
            { pvMessage = "Timestamp must be an integer (not float)"
            , pvSeverity = Error
            , pvLocation = InMessage (Just 42) 1 (Just "metadata.timestamp")
            , pvExpected = Just "integer (use Math.floor(Date.now() / 1000) in JavaScript)"
            , pvActual = Just "1234567890.5"
            , pvFix = Just "Convert timestamp to integer: Math.floor(Date.now() / 1000)"
            }
        , ProtocolViolation
            { pvMessage = "Consider using progress messages for long-running operations"
            , pvSeverity = Info
            , pvLocation = InStream "process.task" 0
            , pvExpected = Nothing
            , pvActual = Nothing
            , pvFix = Just "Send periodic progress updates to improve UX"
            }
        ]

  -- 1. Compiler-style format
  putStrLn "===================================================================\n"
  putStrLn "1. COMPILER-STYLE FORMAT (renderViolations)"
  putStrLn "===================================================================\n"
  TIO.putStrLn $ renderViolations violations
  putStrLn ""

  -- 2. Summary format
  putStrLn "===================================================================\n"
  putStrLn "2. SUMMARY FORMAT (renderSummary)"
  putStrLn "===================================================================\n"
  TIO.putStrLn $ renderSummary violations
  putStrLn ""

  -- 3. Compact format
  putStrLn "===================================================================\n"
  putStrLn "3. COMPACT FORMAT (renderCompact)"
  putStrLn "===================================================================\n"
  TIO.putStrLn $ renderCompact violations
  putStrLn ""

  -- 4. JSON format
  putStrLn "===================================================================\n"
  putStrLn "4. JSON FORMAT (renderJSON)"
  putStrLn "===================================================================\n"
  TIO.putStrLn $ renderJSON violations
  putStrLn ""

  -- Test with no violations
  putStrLn "===================================================================\n"
  putStrLn "5. NO VIOLATIONS (all formats)"
  putStrLn "===================================================================\n"
  TIO.putStrLn $ renderViolations []
  TIO.putStrLn $ renderSummary []
  TIO.putStrLn $ renderCompact []
  putStrLn ""
