-- | Network-related types with refined constraints
module Synapse.Types.Network
  ( -- * Connection
    ConnectionId
  , mkConnectionId
  , unConnectionId

    -- * URL Components
  , Host
  , mkHost
  , unHost

    -- * Re-exports
  , Port
  , mkPort
  , mkPortUnsafe
  , unPort
  ) where

import Data.Text (Text)
import Refined
import Synapse.Types.Refined (Port, mkPort, mkPortUnsafe, unPort)

-------------------------------------------------------------------------------
-- Connection ID

-- | Connection identifier (non-negative)
type ConnectionId = Refined NonNegative Int

-- | Smart constructor for ConnectionId
mkConnectionId :: Int -> Either RefineException ConnectionId
mkConnectionId = refine

-- | Extract underlying Int
unConnectionId :: ConnectionId -> Int
unConnectionId = unrefine

-------------------------------------------------------------------------------
-- Host

-- | Host name (non-empty)
--
-- Examples: @localhost@, @127.0.0.1@, @substrate.local@
type Host = Refined NonEmpty Text

-- | Smart constructor for Host
mkHost :: Text -> Either RefineException Host
mkHost = refine

-- | Extract underlying Text
unHost :: Host -> Text
unHost = unrefine
