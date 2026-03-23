{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Structured logging for Synapse using fast-logger
--
-- Provides filterable, subsystem-tagged logging with three levels:
-- - Info: High-level operations (connections, discoveries, invocations)
-- - Debug: Detailed flow (schema fetches, cache hits/misses)
-- - Trace: Very detailed (raw messages, packet-level details)
module Synapse.Log
  ( -- * Types
    LogLevel(..)
  , Subsystem(..)
  , Logger(..)

    -- * Logger Creation
  , makeLogger
  , nullLogger
  , withLogger

    -- * Logging Functions
  , logInfo
  , logDebug
  , logTrace
  , logWith
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import System.Log.FastLogger
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List (intercalate)

-- | Log levels (ordered from least to most verbose)
data LogLevel
  = Info   -- ^ High-level operations
  | Debug  -- ^ Detailed flow
  | Trace  -- ^ Very detailed (packet-level)
  deriving (Show, Eq, Ord)

-- | Subsystems for filtering
data Subsystem
  = SubsystemDiscovery   -- ^ Backend discovery and health checks
  | SubsystemTransport   -- ^ Network transport and WebSocket
  | SubsystemRPC         -- ^ JSON-RPC layer
  | SubsystemSchema      -- ^ Schema fetching and parsing
  | SubsystemCache       -- ^ Schema caching
  | SubsystemNavigation  -- ^ Path navigation and resolution
  | SubsystemRendering   -- ^ Output rendering
  | SubsystemCLI         -- ^ CLI parsing and execution
  | SubsystemBidir       -- ^ Bidirectional communication
  deriving (Show, Eq)

-- | Logger with filtering
data Logger = Logger
  { logMinLevel     :: !LogLevel
  , logSubsystems   :: !(Maybe [Subsystem])  -- Nothing = all
  , logTimedLogger  :: !TimedFastLogger
  , logCleanup      :: !(IO ())
  }

-- | Create a logger with filtering
makeLogger :: LogLevel -> Maybe [Subsystem] -> IO Logger
makeLogger minLevel mSubsystems = do
  timeCache <- newTimeCache simpleTimeFormat
  (logger, cleanup) <- newTimedFastLogger timeCache (LogStderr defaultBufSize)
  pure Logger
    { logMinLevel = minLevel
    , logSubsystems = mSubsystems
    , logTimedLogger = logger
    , logCleanup = cleanup
    }

-- | A null logger that discards all messages
nullLogger :: Logger
nullLogger = Logger
  { logMinLevel = Info
  , logSubsystems = Nothing
  , logTimedLogger = \_ -> pure ()  -- Discard the (FormattedTime -> LogStr)
  , logCleanup = pure ()
  }

-- | Run an action with a logger, ensuring cleanup
withLogger :: LogLevel -> Maybe [Subsystem] -> (Logger -> IO a) -> IO a
withLogger level subs action = do
  logger <- makeLogger level subs
  result <- action logger
  logCleanup logger
  pure result

-- | Log a message with fields
logWith :: MonadIO m => Logger -> LogLevel -> Subsystem -> Text -> [(Text, Text)] -> m ()
logWith Logger{..} level subsys msg fields = liftIO $
  when (shouldLog level subsys) $ do
    let levelStr = formatLevel level
    let subsysStr = formatSubsystem subsys
    let fieldsStr = if null fields then "" else " {" <> T.intercalate ", " (map formatField fields) <> "}"
    let logLine = toLogStr $
          "[" <> levelStr <> "] " <>
          "[" <> subsysStr <> "] " <>
          msg <> fieldsStr <> "\n"
    logTimedLogger (\_time -> logLine)
  where
    shouldLog lvl sub =
      lvl >= logMinLevel &&
      case logSubsystems of
        Nothing -> True
        Just subs -> sub `elem` subs

    formatLevel Info = "INFO "
    formatLevel Debug = "DEBUG"
    formatLevel Trace = "TRACE"

    formatSubsystem SubsystemDiscovery  = "discovery "
    formatSubsystem SubsystemTransport  = "transport "
    formatSubsystem SubsystemRPC        = "rpc       "
    formatSubsystem SubsystemSchema     = "schema    "
    formatSubsystem SubsystemCache      = "cache     "
    formatSubsystem SubsystemNavigation = "navigation"
    formatSubsystem SubsystemRendering  = "rendering "
    formatSubsystem SubsystemCLI        = "cli       "
    formatSubsystem SubsystemBidir      = "bidir     "

    formatField (k, v) = k <> "=" <> v

    when cond action = if cond then action else pure ()

-- | Log an info-level message
logInfo :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logInfo logger subsys msg = logWith logger Info subsys msg []

-- | Log a debug-level message
logDebug :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logDebug logger subsys msg = logWith logger Debug subsys msg []

-- | Log a trace-level message
logTrace :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logTrace logger subsys msg = logWith logger Trace subsys msg []
