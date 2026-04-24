{-# LANGUAGE OverloadedStrings #-}

-- | The @file:\/\/\<path\>@ resolver — read credentials from a file.
--
-- Generalization of the existing @--token-file@ idiom. The path is
-- reconstructed from the parsed URI (authority ++ path), supports @~@
-- expansion, and a single trailing newline is stripped so cleanly
-- concatenated @printf \'%s\\n\' \> token@ files Just Work. Interior
-- whitespace is preserved.
--
-- Error mapping:
--
-- * Missing file → 'ResolveNotFound'
-- * Other IO errors (permission denied, is-a-directory, …) →
--   'ResolveBackendError' with the IOError message.
module Synapse.Self.Resolve.File
  ( fileResolver
  ) where

import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getHomeDirectory)
import System.IO.Error (isDoesNotExistError)

import Synapse.Self.Resolve
  ( ParsedUri(..)
  , ParsedUriBody(..)
  , ResolveError(..)
  , ResolveFn
  )
import Synapse.Self.Types (CredentialRef(..))

-- | Resolver for @file:\/\/\<path\>@.
--
-- Dispatches on 'HierarchicalBody'. Reconstructs the path as
-- @authority ++ path@, expands a leading @~@ to the user\'s home
-- directory, reads the file as UTF-8, and strips a single trailing
-- newline.
fileResolver :: ResolveFn
fileResolver ParsedUri{puScheme, puBody} =
  case puBody of
    HierarchicalBody{puAuthority = auth, puPath = p} -> do
      let raw       = auth <> p
          canonical = CredentialRef (puScheme <> "://" <> raw)
      if T.null raw
        then pure $ Left $ ResolveBackendError canonical
               "file: missing path (expected file:///abs or file://~/rel)"
        else do
          expanded <- expandHome (T.unpack raw)
          readWithStrip canonical expanded
    OpaqueBody body ->
      pure $ Left $ ResolveBackendError
        (CredentialRef (puScheme <> ":" <> body))
        "file: scheme must be hierarchical (file://path), not opaque"

-- | Expand a leading @~@ or @~\/@ to the user\'s home directory. Any
-- other @~@ (e.g. @~user@) is left alone — that form would need
-- platform-specific getpwnam logic we don\'t want to pull in here.
expandHome :: FilePath -> IO FilePath
expandHome path = case path of
  "~"             -> getHomeDirectory
  '~':'/':rest    -> do
    home <- getHomeDirectory
    pure (home <> "/" <> rest)
  _               -> pure path

-- | Read @path@ and strip a single trailing @\\n@ if present. All other
-- bytes (including interior newlines and leading\/trailing spaces) are
-- preserved verbatim.
readWithStrip :: CredentialRef -> FilePath -> IO (Either ResolveError T.Text)
readWithStrip ref path = do
  result <- try (TIO.readFile path) :: IO (Either IOException T.Text)
  case result of
    Right contents -> pure $ Right (stripTrailingNewline contents)
    Left e
      | isDoesNotExistError e -> pure $ Left $ ResolveNotFound ref
      | otherwise ->
          pure $ Left $ ResolveBackendError ref (T.pack (show e))

-- | Strip a single trailing @\\n@. No-op if the file has no trailing
-- newline, and only one is removed even if the file ends in @\\n\\n@.
stripTrailingNewline :: T.Text -> T.Text
stripTrailingNewline t = case T.unsnoc t of
  Just (prefix, '\n') -> prefix
  _                   -> t
