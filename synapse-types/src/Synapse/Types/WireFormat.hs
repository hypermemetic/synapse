{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Wire format types with GADTs enforcing metadata requirements
--
-- This module uses GADTs to enforce at the type level which stream
-- items require metadata. Request items do NOT have metadata, all
-- others DO. This prevents bugs where metadata is forgotten.
module Synapse.Types.WireFormat
  ( -- * Stream Items (GADT)
    StreamItem(..)
  , ItemType(..)

    -- * Helper Types
  , ErrorCode
  , mkErrorCode
  , TimeoutMs
  , mkTimeoutMs
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Refined
import Synapse.Types.Protocol
import Synapse.Types.Refined

-------------------------------------------------------------------------------
-- Item Type (Type-Level)

-- | Stream item type at the type level
--
-- Used as a phantom type parameter to StreamItem GADT
data ItemType
  = TData      -- ^ Data item (metadata required)
  | TError     -- ^ Error item (metadata required)
  | TDone      -- ^ Done item (metadata required)
  | TProgress  -- ^ Progress item (metadata required)
  | TRequest   -- ^ Request item (NO metadata)

-------------------------------------------------------------------------------
-- Stream Item GADT

-- | Stream item with metadata requirements enforced by GADT
--
-- The type parameter indicates what kind of item this is,
-- and the GADT structure ensures metadata is present only
-- where required.
--
-- Example:
--
-- @
-- -- ✅ DataItem requires metadata
-- dataItem :: StreamItem 'TData
-- dataItem = DataItem metadata contentType content
--
-- -- ✅ RequestItem does NOT have metadata field
-- reqItem :: StreamItem 'TRequest
-- reqItem = RequestItem requestId requestData timeout
--
-- -- ❌ This won't compile (missing metadata):
-- -- bad = DataItem contentType content
-- @
data StreamItem (t :: ItemType) where
  -- | Data item with metadata
  --
  -- Contains actual response data from a method call.
  DataItem
    :: { diMetadata    :: !StreamMetadata  -- ^ Metadata (required!)
       , diContentType :: !ContentType     -- ^ Content type (snake_case)
       , diContent     :: !Value           -- ^ Actual data payload
       }
    -> StreamItem 'TData

  -- | Error item with metadata
  --
  -- Terminates subscription with an error.
  ErrorItem
    :: { eiMetadata    :: !StreamMetadata  -- ^ Metadata (required!)
       , eiMessage     :: !Text            -- ^ Error message
       , eiCode        :: !(Maybe ErrorCode)  -- ^ Optional error code
       , eiRecoverable :: !Bool            -- ^ Whether error is recoverable
       }
    -> StreamItem 'TError

  -- | Done item with metadata
  --
  -- Terminates subscription successfully.
  DoneItem
    :: { doneMetadata :: !StreamMetadata   -- ^ Metadata (required!)
       }
    -> StreamItem 'TDone

  -- | Progress item with metadata
  --
  -- Reports progress of long-running operation.
  ProgressItem
    :: { piMetadata   :: !StreamMetadata   -- ^ Metadata (required!)
       , piMessage    :: !Text             -- ^ Progress message
       , piPercentage :: !Percentage       -- ^ Progress (0-100)
       }
    -> StreamItem 'TProgress

  -- | Request item (bidirectional - NO metadata!)
  --
  -- Server requests something from client during subscription.
  -- This is the ONLY item type without metadata.
  RequestItem
    :: { riRequestId   :: !Text            -- ^ Request identifier
       , riRequestData :: !Value           -- ^ Request payload
       , riTimeoutMs   :: !TimeoutMs       -- ^ Timeout in milliseconds
       }
    -> StreamItem 'TRequest
    -- ☝️ Notice: NO metadata field!

-- Deriving Show for GADT requires StandaloneDeriving
deriving instance Show (StreamItem t)
deriving instance Eq (StreamItem t)

-------------------------------------------------------------------------------
-- Helper Types

-- | Error code (snake_case)
type ErrorCode = SnakeCaseField

-- | Smart constructor for ErrorCode
mkErrorCode :: Text -> Either RefineException ErrorCode
mkErrorCode = mkSnakeCaseField

-- | Timeout in milliseconds (positive)
type TimeoutMs = Refined Positive Int

-- | Smart constructor for TimeoutMs
mkTimeoutMs :: Int -> Either RefineException TimeoutMs
mkTimeoutMs = refine

-------------------------------------------------------------------------------
-- Examples and Documentation

-- $examples
--
-- = Creating stream items
--
-- @
-- import Synapse.Types.WireFormat
-- import Synapse.Types.Protocol
-- import Synapse.Types.Refined
--
-- -- Create metadata
-- Right meta <- mkStreamMetadata "substrate" "abc123def4567890" 1735052400
-- Right contentType <- mkContentType "solar.result"
--
-- -- ✅ Create data item (metadata required)
-- dataItem :: StreamItem 'TData
-- dataItem = DataItem meta contentType someValue
--
-- -- ✅ Create request item (NO metadata)
-- Right timeout <- mkTimeoutMs 5000
-- reqItem :: StreamItem 'TRequest
-- reqItem = RequestItem "req-123" requestPayload timeout
--
-- -- ❌ This won't compile (missing metadata):
-- -- bad = DataItem contentType someValue
-- --       ^^^ Type error: expected 3 arguments, got 2
-- @
--
-- = Type safety in action
--
-- The GADT structure prevents common bugs:
--
-- 1. **Forgetting metadata**: Won't compile
--
-- @
-- -- ❌ Forgot metadata
-- bad1 = DataItem contentType value
-- -- Type error: Missing metadata argument
-- @
--
-- 2. **Adding metadata to request**: Won't compile
--
-- @
-- -- ❌ Request items don't have metadata
-- bad2 = RequestItem meta "req-id" value timeout
-- -- Type error: Too many arguments
-- @
--
-- 3. **Wrong item type**: Caught by type checker
--
-- @
-- sendDataItem :: StreamItem 'TData -> IO ()
-- sendRequestItem :: StreamItem 'TRequest -> IO ()
--
-- -- ❌ Type mismatch
-- sendDataItem reqItem
-- -- Type error: Expected TData, got TRequest
-- @
