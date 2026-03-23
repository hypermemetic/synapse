{-# LANGUAGE TemplateHaskell #-}

-- | Refined types for Plexus RPC protocol primitives
--
-- This module provides type-safe wrappers around basic types with
-- constraints enforced at construction time. Invalid values cannot
-- be constructed.
--
-- Example:
--
-- @
-- import Synapse.Types.Refined
--
-- -- ✅ Valid hash
-- goodHash :: Either RefineException PlexusHash
-- goodHash = mkPlexusHash "abc123def4567890"
--
-- -- ❌ Invalid hash (wrong length)
-- badHash :: Either RefineException PlexusHash
-- badHash = mkPlexusHash "short"  -- Left (RefineException ...)
-- @
module Synapse.Types.Refined
  ( -- * Hash Types
    PlexusHash
  , mkPlexusHash
  , mkPlexusHashUnsafe
  , unPlexusHash

    -- * Timestamp Types
  , Timestamp
  , mkTimestamp
  , mkTimestampUnsafe
  , unTimestamp
  , currentTimestamp

    -- * Field Name Types
  , SnakeCaseField
  , mkSnakeCaseField
  , mkSnakeCaseFieldUnsafe
  , unSnakeCaseField

    -- * Namespace Types
  , Namespace
  , mkNamespace
  , mkNamespaceUnsafe
  , unNamespace

    -- * Method Name Types
  , MethodName
  , mkMethodName
  , mkMethodNameUnsafe
  , unMethodName

    -- * Network Types
  , Port
  , mkPort
  , mkPortUnsafe
  , unPort

    -- * ID Types
  , SubscriptionId
  , mkSubscriptionId
  , mkSubscriptionIdUnsafe
  , unSubscriptionId
  , RequestId
  , mkRequestId
  , mkRequestIdUnsafe
  , unRequestId

    -- * Custom Predicates
  , Hex
  , SnakeCase
  , ValidIdentifier
  , Positive

    -- * Re-exports from refined
  , Refined
  , refine
  , unrefine
  , RefineException(..)
  ) where

import Data.Char (isAlphaNum, isDigit, isLower)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (typeOf)
import Refined
import Refined.Unsafe (reallyUnsafeRefine)

-------------------------------------------------------------------------------
-- Custom Predicates

-- | Predicate for hexadecimal strings (lowercase)
data Hex

instance Predicate Hex Text where
  validate p txt =
    if T.all (`elem` ("0123456789abcdef" :: String)) txt
    then Nothing
    else throwRefineOtherException (typeOf p) "Must be lowercase hexadecimal"

-- | Predicate for snake_case strings
data SnakeCase

instance Predicate SnakeCase Text where
  validate p txt =
    let str = T.unpack txt
    in if not (null str) && all (\c -> isLower c || isDigit c || c == '_') str
                         && head str /= '_'
                         && isLower (head str)
       then Nothing
       else throwRefineOtherException (typeOf p) "Must be snake_case (lowercase letters, digits, underscores)"

-- | Predicate for valid identifiers (letters, digits, underscores)
data ValidIdentifier

instance Predicate ValidIdentifier Text where
  validate p txt =
    let str = T.unpack txt
    in if not (null str) && isLower (head str) && all (\c -> isAlphaNum c || c == '_') str
       then Nothing
       else throwRefineOtherException (typeOf p) "Must be valid identifier (start with lowercase letter)"

-- Note: Using Positive from Refined library (not defining our own)

-------------------------------------------------------------------------------
-- Hash Types

-- | Plexus hash: exactly 16 lowercase hexadecimal characters
--
-- Represents SHA-256 hash truncated to first 64 bits (16 hex chars).
-- Used to identify schema versions and configurations.
--
-- Example: @"abc123def4567890"@
type PlexusHash = Refined (SizeEqualTo 16 `And` Hex) Text

-- | Smart constructor for PlexusHash
mkPlexusHash :: Text -> Either RefineException PlexusHash
mkPlexusHash = refine

-- | Unsafe constructor (throws on invalid input)
mkPlexusHashUnsafe :: Text -> PlexusHash
mkPlexusHashUnsafe = reallyUnsafeRefine

-- | Extract underlying Text
unPlexusHash :: PlexusHash -> Text
unPlexusHash = unrefine

-------------------------------------------------------------------------------
-- Timestamp Types

-- | Unix timestamp in seconds (positive Int64)
--
-- CRITICAL: Must be integer, not float!
-- The float timestamp bug (Date.now() / 1000) breaks Haskell/Rust parsers.
-- Use Math.floor(Date.now() / 1000) in JavaScript.
type Timestamp = Refined Positive Int64

-- | Smart constructor for Timestamp
mkTimestamp :: Int64 -> Either RefineException Timestamp
mkTimestamp = refine

-- | Unsafe constructor (throws on invalid input)
mkTimestampUnsafe :: Int64 -> Timestamp
mkTimestampUnsafe = reallyUnsafeRefine

-- | Extract underlying Int64
unTimestamp :: Timestamp -> Int64
unTimestamp = unrefine

-- | Get current timestamp (always valid)
currentTimestamp :: IO Timestamp
currentTimestamp = do
  t <- getPOSIXTime
  return $ mkTimestampUnsafe (floor t)

-------------------------------------------------------------------------------
-- Field Name Types

-- | Field name in snake_case format (wire format requirement)
--
-- Wire format requires snake_case, not camelCase:
-- - Correct: @plexus_hash@, @content_type@, @request_id@
-- - Wrong: @plexusHash@, @contentType@, @requestId@
type SnakeCaseField = Refined SnakeCase Text

-- | Smart constructor for SnakeCaseField
mkSnakeCaseField :: Text -> Either RefineException SnakeCaseField
mkSnakeCaseField = refine

-- | Unsafe constructor (throws on invalid input)
mkSnakeCaseFieldUnsafe :: Text -> SnakeCaseField
mkSnakeCaseFieldUnsafe = reallyUnsafeRefine

-- | Extract underlying Text
unSnakeCaseField :: SnakeCaseField -> Text
unSnakeCaseField = unrefine

-- Common field names (compile-time checked)
-- Note: Due to TH stage restrictions, these examples are commented out.
-- In real code, define these in a separate module to use TH.
--
-- plexusHashField :: SnakeCaseField
-- plexusHashField = $$(refineTH "plexus_hash")
--
-- contentTypeField :: SnakeCaseField
-- contentTypeField = $$(refineTH "content_type")

-------------------------------------------------------------------------------
-- Namespace Types

-- | Namespace: non-empty, lowercase, dots allowed
--
-- Examples: @cone@, @cone.chat@, @substrate.health@
type Namespace = Refined (NonEmpty `And` ValidIdentifier) Text

-- | Smart constructor for Namespace
mkNamespace :: Text -> Either RefineException Namespace
mkNamespace = refine

-- | Unsafe constructor (throws on invalid input)
mkNamespaceUnsafe :: Text -> Namespace
mkNamespaceUnsafe = reallyUnsafeRefine

-- | Extract underlying Text
unNamespace :: Namespace -> Text
unNamespace = unrefine

-------------------------------------------------------------------------------
-- Method Name Types

-- | Method name: valid identifier
--
-- Examples: @chat@, @ping@, @get_status@
type MethodName = Refined (NonEmpty `And` ValidIdentifier) Text

-- | Smart constructor for MethodName
mkMethodName :: Text -> Either RefineException MethodName
mkMethodName = refine

-- | Unsafe constructor (throws on invalid input)
mkMethodNameUnsafe :: Text -> MethodName
mkMethodNameUnsafe = reallyUnsafeRefine

-- | Extract underlying Text
unMethodName :: MethodName -> Text
unMethodName = unrefine

-------------------------------------------------------------------------------
-- Network Types

-- | TCP/UDP port number (1-65535)
type Port = Refined (From 1 `And` To 65535) Int

-- | Smart constructor for Port
mkPort :: Int -> Either RefineException Port
mkPort = refine

-- | Unsafe constructor (throws on invalid input)
mkPortUnsafe :: Int -> Port
mkPortUnsafe = reallyUnsafeRefine

-- | Extract underlying Int
unPort :: Port -> Int
unPort = unrefine

-------------------------------------------------------------------------------
-- ID Types

-- | Subscription ID (non-negative)
type SubscriptionId = Refined NonNegative Int

-- | Smart constructor for SubscriptionId
mkSubscriptionId :: Int -> Either RefineException SubscriptionId
mkSubscriptionId = refine

-- | Unsafe constructor (throws on invalid input)
mkSubscriptionIdUnsafe :: Int -> SubscriptionId
mkSubscriptionIdUnsafe = reallyUnsafeRefine

-- | Extract underlying Int
unSubscriptionId :: SubscriptionId -> Int
unSubscriptionId = unrefine

-- | Request ID (positive)
type RequestId = Refined Positive Int

-- | Smart constructor for RequestId
mkRequestId :: Int -> Either RefineException RequestId
mkRequestId = refine

-- | Unsafe constructor (throws on invalid input)
mkRequestIdUnsafe :: Int -> RequestId
mkRequestIdUnsafe = reallyUnsafeRefine

-- | Extract underlying Int
unRequestId :: RequestId -> Int
unRequestId = unrefine
