{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Synapse.Self.Protocol.Reporter
  ( -- * Main rendering functions
    renderViolations
  , renderSummary
  , renderCompact
  , renderJSON

    -- * Helper functions
  , groupBySeverity
  , formatLocation
  , severitySymbol
  , severityName
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Synapse.Self.Protocol.Validator
  ( ProtocolViolation(..)
  , Severity(..)
  , Location(..)
  )

-- ============================================================================
-- Main Rendering Functions
-- ============================================================================

-- | Render violations in compiler-style format
--
-- Example output:
-- > error: Missing StreamDone message
-- >   --> stream:subscription_42
-- >   |
-- >   | Stream received 5 data messages but never received StreamDone
-- >   | Expected: StreamDone message to complete stream
-- >   | Fix: Ensure server sends StreamDone after all data
renderViolations :: [ProtocolViolation] -> Text
renderViolations [] = "No protocol violations found.\n"
renderViolations violations =
  let grouped = groupBySeverity violations
      builder = TB.fromText "Protocol Violations:\n"
             <> TB.fromText "\n"
             <> renderGroup "Errors" Error grouped
             <> renderGroup "Warnings" Warning grouped
             <> renderGroup "Info" Info grouped
  in TL.toStrict $ TB.toLazyText builder

-- | Render a group of violations by severity
renderGroup :: Text -> Severity -> [(Severity, [ProtocolViolation])] -> TB.Builder
renderGroup label sev groups =
  case lookup sev groups of
    Nothing -> mempty
    Just [] -> mempty
    Just vs ->
      TB.fromText label
      <> TB.fromText ":\n"
      <> TB.fromText "----------------------------------------\n\n"
      <> mconcat (map renderSingleViolation vs)
      <> TB.fromText "\n"

-- | Render a single violation in compiler style
renderSingleViolation :: ProtocolViolation -> TB.Builder
renderSingleViolation ProtocolViolation{..} =
  let sevName = T.toLower $ severityName pvSeverity
      locText = formatLocation pvLocation

      -- Build the main error line
      mainLine = severitySymbol pvSeverity
              <> " "
              <> sevName
              <> ": "
              <> pvMessage
              <> "\n"

      -- Build the location line
      locLine = if T.null locText
                  then mempty
                  else TB.fromText "  --> " <> TB.fromText locText <> TB.fromText "\n"

      -- Build the separator
      separator = if T.null locText
                    then mempty
                    else TB.fromText "  |\n"

      -- Build the details section
      details = buildDetails pvExpected pvActual pvFix

  in TB.fromText mainLine
     <> locLine
     <> separator
     <> details
     <> TB.fromText "\n"

-- | Build the details section with expected/actual/fix
buildDetails :: Maybe Text -> Maybe Text -> Maybe Text -> TB.Builder
buildDetails expected actual fix =
  let expectedLine = case expected of
        Nothing -> mempty
        Just exp -> TB.fromText "  | Expected: " <> TB.fromText exp <> TB.fromText "\n"

      actualLine = case actual of
        Nothing -> mempty
        Just act -> TB.fromText "  | Actual: " <> TB.fromText act <> TB.fromText "\n"

      fixLine = case fix of
        Nothing -> mempty
        Just f -> TB.fromText "  | Fix: " <> TB.fromText f <> TB.fromText "\n"

  in expectedLine <> actualLine <> fixLine

-- | Render summary of violations
--
-- Example output:
-- > Protocol Validation Results:
-- >   ✗ 2 errors
-- >   ⚠ 1 warning
-- >   ℹ 0 info
-- >
-- > FAILED: Protocol violations detected
renderSummary :: [ProtocolViolation] -> Text
renderSummary violations =
  let grouped = groupBySeverity violations
      errorCount = maybe 0 length $ lookup Error grouped
      warningCount = maybe 0 length $ lookup Warning grouped
      infoCount = maybe 0 length $ lookup Info grouped

      status = if errorCount > 0
                 then "FAILED: Protocol violations detected"
                 else if warningCount > 0
                   then "PASSED with warnings"
                   else "PASSED: No protocol violations"

      builder = TB.fromText "Protocol Validation Results:\n"
             <> TB.fromText "  " <> TB.fromText (severitySymbol Error) <> TB.fromText " "
             <> TB.fromString (show errorCount) <> TB.fromText " errors\n"
             <> TB.fromText "  " <> TB.fromText (severitySymbol Warning) <> TB.fromText " "
             <> TB.fromString (show warningCount) <> TB.fromText " warnings\n"
             <> TB.fromText "  " <> TB.fromText (severitySymbol Info) <> TB.fromText " "
             <> TB.fromString (show infoCount) <> TB.fromText " info\n"
             <> TB.fromText "\n"
             <> TB.fromText status
             <> TB.fromText "\n"

  in TL.toStrict $ TB.toLazyText builder

-- | Render violations in compact one-line format
--
-- Example output:
-- > [ERROR] Missing StreamDone message at stream:subscription_42: Stream received 5 data messages
renderCompact :: [ProtocolViolation] -> Text
renderCompact [] = "No protocol violations found.\n"
renderCompact violations =
  let sorted = sortOn (Down . pvSeverity) violations
      lines = map renderCompactLine sorted
  in T.intercalate "\n" lines <> "\n"

-- | Render a single violation in compact format
renderCompactLine :: ProtocolViolation -> Text
renderCompactLine ProtocolViolation{..} =
  let sevName = T.toUpper $ severityName pvSeverity
      locText = formatLocation pvLocation

      -- Build the compact line
      line = "["
          <> sevName
          <> "] "
          <> pvMessage

      -- Add location if present
      withLoc = if T.null locText
                  then line
                  else line <> " at " <> locText

      -- Add details if present
      withDetails = case (pvExpected, pvActual) of
        (Just exp, Just act) -> withLoc <> ": expected " <> exp <> ", got " <> act
        (Just exp, Nothing) -> withLoc <> ": expected " <> exp
        (Nothing, Just act) -> withLoc <> ": got " <> act
        (Nothing, Nothing) -> withLoc

  in withDetails

-- | Render violations in JSON format for machine consumption
--
-- Example output:
-- > {
-- >   "violations": [
-- >     {
-- >       "message": "Missing StreamDone message",
-- >       "severity": "error",
-- >       "location": {"type": "stream", "method": "echo", "message_count": 5},
-- >       "expected": "StreamDone message to complete stream",
-- >       "actual": null,
-- >       "fix": "Ensure server sends StreamDone after all data"
-- >     }
-- >   ],
-- >   "summary": {
-- >     "errors": 1,
-- >     "warnings": 0,
-- >     "info": 0,
-- >     "passed": false
-- >   }
-- > }
renderJSON :: [ProtocolViolation] -> Text
renderJSON violations =
  let grouped = groupBySeverity violations
      errorCount = maybe 0 length $ lookup Error grouped
      warningCount = maybe 0 length $ lookup Warning grouped
      infoCount = maybe 0 length $ lookup Info grouped

      violationsArray = map violationToJSON violations

      summaryObj = object
        [ "errors" .= errorCount
        , "warnings" .= warningCount
        , "info" .= infoCount
        , "passed" .= (errorCount == 0)
        ]

      rootObj = object
        [ "violations" .= violationsArray
        , "summary" .= summaryObj
        ]

  in TE.decodeUtf8 $ BL.toStrict $ Aeson.encode rootObj

-- | Convert a violation to JSON value
violationToJSON :: ProtocolViolation -> Value
violationToJSON ProtocolViolation{..} =
  object
    [ "message" .= pvMessage
    , "severity" .= severityToJSON pvSeverity
    , "location" .= locationToJSON pvLocation
    , "expected" .= pvExpected
    , "actual" .= pvActual
    , "fix" .= pvFix
    ]

-- | Convert severity to JSON string
severityToJSON :: Severity -> Text
severityToJSON Error = "error"
severityToJSON Warning = "warning"
severityToJSON Info = "info"

-- | Convert location to JSON object
locationToJSON :: Location -> Value
locationToJSON (InMessage subId msgIdx field) =
  object
    [ "type" .= ("message" :: Text)
    , "subscription_id" .= subId
    , "message_index" .= msgIdx
    , "field" .= field
    ]
locationToJSON (InStream method msgCount) =
  object
    [ "type" .= ("stream" :: Text)
    , "method" .= method
    , "message_count" .= msgCount
    ]
locationToJSON InConnection =
  object
    [ "type" .= ("connection" :: Text)
    ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Group violations by severity (highest severity first)
groupBySeverity :: [ProtocolViolation] -> [(Severity, [ProtocolViolation])]
groupBySeverity violations =
  let errors = filter (\v -> pvSeverity v == Error) violations
      warnings = filter (\v -> pvSeverity v == Warning) violations
      infos = filter (\v -> pvSeverity v == Info) violations
  in [ (Error, errors)
     , (Warning, warnings)
     , (Info, infos)
     ]

-- | Format location for display
formatLocation :: Location -> Text
formatLocation (InMessage subId msgIdx field) =
  let subPart = case subId of
        Nothing -> "message"
        Just sid -> "subscription_" <> T.pack (show sid)

      msgPart = "msg_" <> T.pack (show msgIdx)

      fieldPart = case field of
        Nothing -> ""
        Just f -> "." <> f

  in subPart <> ":" <> msgPart <> fieldPart

formatLocation (InStream method msgCount) =
  "stream:" <> method <> " (after " <> T.pack (show msgCount) <> " messages)"

formatLocation InConnection =
  "connection"

-- | Get symbol for severity (for visual distinction)
severitySymbol :: Severity -> Text
severitySymbol Error = "X"
severitySymbol Warning = "!"
severitySymbol Info = "*"

-- | Get name for severity
severityName :: Severity -> Text
severityName Error = "Error"
severityName Warning = "Warning"
severityName Info = "Info"
