{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | On-disk types and file format for per-backend defaults.
--
-- The defaults store is a manifest of credential *references* (URIs),
-- not plaintext credentials. A resolver registry ('Synapse.Self.Resolve')
-- dereferences the URIs at dispatch time.
--
-- This module defines:
--
-- * 'StoredDefaults' — in-memory shape of the on-disk file.
-- * 'CredentialRef' — an opaque URI string carried verbatim.
-- * 'ScopedDefaults' — reserved for per-namespace / per-method overrides;
--   parsed but unused in v1.
-- * Pure 'encodeDefaults' / 'decodeDefaults' functions with deterministic
--   key ordering and two-space indent.
--
-- The file format is versioned; unknown top-level fields are tolerated for
-- forward compatibility, but unknown versions are rejected loudly.
module Synapse.Self.Types
  ( -- * On-disk shape
    StoredDefaults(..)
  , CredentialRef(..)
  , ScopedDefaults(..)
  , emptyStoredDefaults
  , currentVersion

    -- * Encoding
  , encodeDefaults
  , decodeDefaults
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), (.!=), object, withObject, withText)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), NumberFormat(..), encodePretty')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Current on-disk format version.
--
-- Incremented whenever the file shape changes in a non-backward-compatible
-- way. Decoders reject anything that is not exactly this version with a
-- structured error; see 'decodeDefaults'.
currentVersion :: Int
currentVersion = 1

-- | A raw credential reference URI, carried verbatim.
--
-- The wrapper exists so we never accidentally confuse a URI with a resolved
-- secret value. Parsing into scheme + body is deferred to 'Synapse.Self.Resolve.parseUri'
-- because the file parses as long as the shape is right — invalid schemes
-- should only surface when we try to actually resolve them.
newtype CredentialRef = CredentialRef { unCredentialRef :: Text }
  deriving stock (Show, Eq, Ord)

instance ToJSON CredentialRef where
  toJSON = String . unCredentialRef

instance FromJSON CredentialRef where
  parseJSON = withText "CredentialRef" (pure . CredentialRef)

-- | Per-scope overrides. v1 carries the shape through verbatim but does
-- not consume it — resolution ignores scopes for now. Defined here so the
-- file format is stable when scope semantics land.
data ScopedDefaults = ScopedDefaults
  { sdsCookies :: Map Text CredentialRef
  , sdsHeaders :: Map Text CredentialRef
  }
  deriving stock (Show, Eq)

instance ToJSON ScopedDefaults where
  toJSON ScopedDefaults{..} = object
    [ "cookies" .= sdsCookies
    , "headers" .= sdsHeaders
    ]

instance FromJSON ScopedDefaults where
  parseJSON = withObject "ScopedDefaults" $ \o -> do
    cookies <- o .:? "cookies" .!= Map.empty
    headers <- o .:? "headers" .!= Map.empty
    pure ScopedDefaults
      { sdsCookies = cookies
      , sdsHeaders = headers
      }

-- | The on-disk defaults store for a single backend.
--
-- Values in 'sdCookies' / 'sdHeaders' are 'CredentialRef's — URIs like
-- @keychain:\/\/svc\/account@ or @literal:raw-value@. The resolver registry
-- dereferences them at dispatch time.
data StoredDefaults = StoredDefaults
  { sdVersion :: Int
  , sdCookies :: Map Text CredentialRef
  , sdHeaders :: Map Text CredentialRef
  , sdScopes  :: Map Text ScopedDefaults
  }
  deriving stock (Show, Eq)

-- | An empty defaults file at the current version.
emptyStoredDefaults :: StoredDefaults
emptyStoredDefaults = StoredDefaults
  { sdVersion = currentVersion
  , sdCookies = Map.empty
  , sdHeaders = Map.empty
  , sdScopes  = Map.empty
  }

instance ToJSON StoredDefaults where
  toJSON StoredDefaults{..} = object
    [ "version"  .= sdVersion
    , "defaults" .= object
        [ "cookies" .= sdCookies
        , "headers" .= sdHeaders
        ]
    , "scopes"   .= sdScopes
    ]

instance FromJSON StoredDefaults where
  parseJSON = withObject "StoredDefaults" $ \o -> do
    version <- o .: "version"
    defaultsObj <- o .:? "defaults" .!= object []
    (cookies, headers) <- flip (withObject "defaults") defaultsObj $ \d -> do
      c <- d .:? "cookies" .!= Map.empty
      h <- d .:? "headers" .!= Map.empty
      pure (c, h)
    scopes <- o .:? "scopes" .!= Map.empty
    pure StoredDefaults
      { sdVersion = version
      , sdCookies = cookies
      , sdHeaders = headers
      , sdScopes  = scopes
      }

-- | Serialize 'StoredDefaults' with deterministic key order and two-space
-- indent. Callers can hash the result for change detection and expect
-- byte-stable output across runs.
encodeDefaults :: StoredDefaults -> ByteString
encodeDefaults sd = LBS.toStrict (encodePretty' config (toJSON sd))
  where
    config = Config
      { confIndent          = Spaces 2
      , confCompare         = compare
      , confNumFormat       = Generic
      , confTrailingNewline = False
      }

-- | Parse a defaults file. Tolerates unknown top-level keys for forward
-- compatibility, treats a missing @defaults@ object as empty, and rejects
-- unknown versions with a structured message.
decodeDefaults :: ByteString -> Either Text StoredDefaults
decodeDefaults bs =
  case Aeson.eitherDecodeStrict' bs of
    Left err -> Left (T.pack err)
    Right value -> case value of
      Object o -> do
        version <- lookupInt "version" o
        if version /= currentVersion
          then Left ("unsupported version: " <> T.pack (show version)
                    <> ", expected " <> T.pack (show currentVersion))
          else case Aeson.fromJSON value of
            Aeson.Success sd -> Right sd
            Aeson.Error e    -> Left (T.pack e)
      _ -> Left "expected top-level JSON object"
  where
    lookupInt name o = case KM.lookup (Key.fromText name) o of
      Nothing -> Left ("missing required field: " <> name)
      Just v  -> case Aeson.fromJSON v of
        Aeson.Success n -> Right n
        Aeson.Error e   -> Left ("field " <> name <> ": " <> T.pack e)
