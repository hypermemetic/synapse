-- | Typed API for the Bash activation
module Activation.Bash
  ( -- * Stream Events
    BashEvent(..)
    -- * Operations
  , execute
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import Plexus.Client (PlexusConnection, plexusRpc)
import Plexus.Types (PlexusStreamItem(..))

-- | Events emitted by bash command execution
data BashEvent
  = Stdout { line :: Text }
  | Stderr { line :: Text }
  | Exit { code :: Int }
  deriving stock (Show, Eq, Generic)

instance FromJSON BashEvent where
  parseJSON = withObject "BashEvent" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "stdout" -> Stdout <$> o .: "line"
      "stderr" -> Stderr <$> o .: "line"
      "exit"   -> Exit <$> o .: "code"
      _        -> fail $ "Unknown bash event type: " <> show typ

-- | Execute a bash command and stream the output
execute :: PlexusConnection -> Text -> Stream (Of BashEvent) IO ()
execute conn cmd = S.mapMaybe extractBashEvent $ plexusRpc conn "bash_execute" (toJSON [cmd])

extractBashEvent :: PlexusStreamItem -> Maybe BashEvent
extractBashEvent (StreamData _ contentType dat)
  | contentType == "bash.event" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractBashEvent _ = Nothing
