{-# LANGUAGE OverloadedStrings #-}

-- | The @keychain:\/\/\<service\>\/\<account\>@ resolver — OS-managed
-- credential storage.
--
-- Elevates the credentials store out of plaintext into OS-managed
-- credential storage. With keychain refs, @defaults.json@ is a manifest
-- of references and contains no secrets — safe to commit to personal
-- dotfiles.
--
-- __Platforms:__
--
-- * macOS — full support via the @security@ CLI (shell-out, no FFI).
-- * Linux — stub. Returns 'ResolveBackendError' with a clear
--   "not implemented; PR welcome" message.
-- * Windows — stub. Same.
--
-- Platform dispatch is runtime via 'System.Info.os' so the module
-- compiles identically on every platform.
--
-- __URI form:__
--
-- > keychain://<service>/<account>
--
-- The authority is the keychain service; everything after the leading
-- @\/@ in the path is the account. Slashes in the account are preserved
-- (keychain accepts them); e.g. @keychain:\/\/plexus\/uscis\/cookie\/access_token@
-- resolves with @service = "plexus"@ and @account = "uscis\/cookie\/access_token"@.
--
-- __macOS implementation:__
--
-- Shells out to:
--
-- @
-- security find-generic-password -s \<service\> -a \<account\> -w
-- security add-generic-password  -s \<service\> -a \<account\> -w \<value\> -U
-- security delete-generic-password -s \<service\> -a \<account\>
-- @
--
-- Exit code 44 from @security@ means "item not found" and surfaces as
-- 'ResolveNotFound'. Other non-zero exits surface as 'ResolveBackendError'
-- with the captured stderr. The @security -w@ command appends a single
-- trailing newline to stdout — stripped exactly once.
module Synapse.Self.Resolve.Keychain
  ( -- * Resolver
    keychainResolver

    -- * Mutations (for SELF-4 verbs)
  , storeInKeychain
  , deleteFromKeychain

    -- * Platform probe
  , isKeychainPlatform
  , platformName
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import qualified System.Info as Info
import System.Process (readProcessWithExitCode)

import Synapse.Self.Resolve
  ( ParsedUri(..)
  , ParsedUriBody(..)
  , ResolveError(..)
  , ResolveFn
  )
import Synapse.Self.Types (CredentialRef(..))

-- ============================================================================
-- Platform detection
-- ============================================================================

-- | True iff the current platform has a working keychain backend.
-- Only macOS returns True today.
isKeychainPlatform :: Bool
isKeychainPlatform = Info.os == "darwin"

-- | Human-readable platform name, for error messages.
platformName :: Text
platformName = case Info.os of
  "darwin"   -> "macOS"
  "linux"    -> "Linux"
  "mingw32"  -> "Windows"
  "mingw64"  -> "Windows"
  "cygwin32" -> "Windows"
  other      -> T.pack other

-- ============================================================================
-- Resolver
-- ============================================================================

-- | Resolver for @keychain:\/\/\<service\>\/\<account\>@.
keychainResolver :: ResolveFn
keychainResolver ParsedUri{puScheme, puBody} = case puBody of
  OpaqueBody body ->
    pure $ Left $ ResolveBackendError
      (CredentialRef (puScheme <> ":" <> body))
      "keychain: scheme must be hierarchical (keychain://service/account), not opaque"
  HierarchicalBody{puAuthority = service, puPath = rawPath} ->
    case parseServiceAccount service rawPath of
      Left err -> pure (Left err)
      Right (s, a)
        | isKeychainPlatform -> resolveMacOS s a
        | otherwise          -> pure (Left (stubError (canonicalRef s a)))

-- | Split the ParsedUri pieces into @(service, account)@ or a structured
-- 'ResolveParseError' naming what's missing.
parseServiceAccount
  :: Text            -- ^ authority
  -> Text            -- ^ raw path (leading @\/@ expected)
  -> Either ResolveError (Text, Text)
parseServiceAccount service rawPath
  | T.null service =
      Left $ ResolveParseError
        "service missing in keychain://<service>/<account>"
  | otherwise =
      case T.stripPrefix "/" rawPath of
        Nothing ->
          Left $ ResolveParseError
            "account missing in keychain://<service>/<account>"
        Just account
          | T.null account ->
              Left $ ResolveParseError
                "account missing in keychain://<service>/<account>"
          | otherwise -> Right (service, account)

-- | The canonical URI we echo back in 'ResolveError' payloads.
canonicalRef :: Text -> Text -> CredentialRef
canonicalRef service account =
  CredentialRef ("keychain://" <> service <> "/" <> account)

-- | Error returned on platforms without a keychain backend.
stubError :: CredentialRef -> ResolveError
stubError ref =
  ResolveBackendError ref $ T.concat
    [ "keychain backend not implemented on "
    , platformName
    , "; PR welcome."
    ]

-- ============================================================================
-- macOS: shell out to `security`
-- ============================================================================

-- | macOS exit code returned by @security@ when an item is not found.
-- Documented under @errSecItemNotFound@ in the @security(1)@ manpage.
securityNotFoundExitCode :: Int
securityNotFoundExitCode = 44

-- | Resolve via @security find-generic-password -s \<service\> -a \<account\> -w@.
resolveMacOS :: Text -> Text -> IO (Either ResolveError Text)
resolveMacOS service account = do
  let args =
        [ "find-generic-password"
        , "-s", T.unpack service
        , "-a", T.unpack account
        , "-w"
        ]
      ref = canonicalRef service account
  (code, out, err) <- readProcessWithExitCode "security" args ""
  case code of
    ExitSuccess ->
      pure $ Right (stripOneTrailingNewline (T.pack out))
    ExitFailure c
      | c == securityNotFoundExitCode ->
          pure $ Left (ResolveNotFound ref)
      | otherwise ->
          pure $ Left $ ResolveBackendError ref $ T.concat
            [ "security exited with code "
            , T.pack (show c)
            , renderStderr err
            ]

-- | Strip exactly one trailing newline if present.
-- @security -w@ appends one newline; interior bytes are preserved.
stripOneTrailingNewline :: Text -> Text
stripOneTrailingNewline t = case T.unsnoc t of
  Just (pfx, '\n') -> pfx
  _                -> t

-- | Append stderr to an error message if non-empty.
renderStderr :: String -> Text
renderStderr "" = ""
renderStderr s  = ": " <> T.strip (T.pack s)

-- ============================================================================
-- Mutations (exposed for SELF-4)
-- ============================================================================

-- | Store @value@ under @service@/@account@ in the OS keychain.
--
-- Uses @security add-generic-password -U@ so an existing item is updated
-- in place. Returns @Right ()@ on success. On non-macOS platforms,
-- returns @Left@ with a "not implemented" message.
storeInKeychain
  :: Text            -- ^ service
  -> Text            -- ^ account
  -> Text            -- ^ value to store
  -> IO (Either Text ())
storeInKeychain service account value
  | not isKeychainPlatform = pure $ Left $ T.concat
      [ "keychain backend not implemented on "
      , platformName
      , "; cannot store secret. PR welcome."
      ]
  | otherwise = do
      let args =
            [ "add-generic-password"
            , "-s", T.unpack service
            , "-a", T.unpack account
            , "-w", T.unpack value
            , "-U"
            ]
      (code, _out, err) <- readProcessWithExitCode "security" args ""
      case code of
        ExitSuccess -> pure (Right ())
        ExitFailure c ->
          pure $ Left $ T.concat
            [ "security add-generic-password exited with code "
            , T.pack (show c)
            , renderStderr err
            ]

-- | Delete @service@/@account@ from the OS keychain.
--
-- Exit code 44 (not found) is treated as success — the intent ("no such
-- item remains") is already true.
deleteFromKeychain
  :: Text            -- ^ service
  -> Text            -- ^ account
  -> IO (Either Text ())
deleteFromKeychain service account
  | not isKeychainPlatform = pure $ Left $ T.concat
      [ "keychain backend not implemented on "
      , platformName
      , "; cannot delete secret. PR welcome."
      ]
  | otherwise = do
      let args =
            [ "delete-generic-password"
            , "-s", T.unpack service
            , "-a", T.unpack account
            ]
      (code, _out, err) <- readProcessWithExitCode "security" args ""
      case code of
        ExitSuccess -> pure (Right ())
        ExitFailure c
          | c == securityNotFoundExitCode -> pure (Right ())
          | otherwise ->
              pure $ Left $ T.concat
                [ "security delete-generic-password exited with code "
                , T.pack (show c)
                , renderStderr err
                ]
