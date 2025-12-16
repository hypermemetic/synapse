-- | Typed API for the Health activation
module Activation.Health
  ( -- * Stream Events
    HealthEvent(..)
    -- * Operations
  , check
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import Plexus.Client (PlexusConnection, plexusRpc)
import Plexus.Types (PlexusStreamItem(..))

-- | Events emitted by health check
data HealthEvent
  = Status
      { status        :: Text
      , uptimeSeconds :: Int
      , timestamp     :: Int
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON HealthEvent where
  parseJSON = withObject "HealthEvent" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "status" -> Status
        <$> o .: "status"
        <*> o .: "uptime_seconds"
        <*> o .: "timestamp"
      _ -> fail $ "Unknown health event type: " <> show typ

-- | Check plexus health status
check :: PlexusConnection -> Stream (Of HealthEvent) IO ()
check conn = S.mapMaybe extractHealthEvent $ plexusRpc conn "health_check" (toJSON ([] :: [Value]))

extractHealthEvent :: PlexusStreamItem -> Maybe HealthEvent
extractHealthEvent (StreamData _ _ contentType dat)
  | contentType == "health.event" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractHealthEvent _ = Nothing
