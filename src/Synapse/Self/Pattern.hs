{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching for method filtering
--
-- Supports glob-style patterns with wildcards:
-- - plexus.cone.* - matches all methods in cone
-- - plexus.*.create - matches create methods in all activations
-- - plexus.(cone|arbor).* - regex patterns for complex matching
module Synapse.Self.Pattern
  ( MethodPattern(..)
  , parsePattern
  , matchMethods
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))
import Synapse.IR.Types (MethodDef(..))

-- | A compiled pattern for matching method paths
data MethodPattern = MethodPattern
  { mpRawPattern :: Text    -- ^ Original pattern text
  , mpRegex :: Text         -- ^ Compiled regex pattern
  }
  deriving stock (Show, Eq)

-- | Parse a pattern string into a MethodPattern
-- Converts glob-style wildcards to regex:
--   * -> [^.]+  (matches any segment)
--   . -> \.     (literal dot)
parsePattern :: Text -> Either Text MethodPattern
parsePattern raw = do
  let regex = patternToRegex raw
  Right $ MethodPattern raw regex

-- | Convert a glob-style pattern to a PCRE regex
-- Examples:
--   "plexus.cone.*" -> "^plexus\.cone\.[^.]+$"
--   "plexus.*.create" -> "^plexus\.[^.]+\.create$"
patternToRegex :: Text -> Text
patternToRegex pattern =
  let escaped = T.replace "." "\\." pattern      -- Escape dots first
      withWildcards = T.replace "*" "[^.]+" escaped  -- Replace * with segment matcher
  in "^" <> withWildcards <> "$"                 -- Anchor at both ends

-- | Filter methods that match the pattern
matchMethods :: MethodPattern -> [MethodDef] -> [MethodDef]
matchMethods (MethodPattern _ regex) methods =
  filter (\m -> T.unpack (mdFullPath m) =~ T.unpack regex) methods
