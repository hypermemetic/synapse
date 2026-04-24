{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | The @synapse _self \<backend\> \<verb\> ...@ command tree (SELF-4).
--
-- This module is the user-facing surface of the per-backend defaults store
-- introduced in SELF-1 / SELF-2 / SELF-7. It exposes:
--
-- * A 'SelfCommand' ADT covering every verb in the ticket spec.
-- * A single 'runSelfCommand' entry point that returns an 'ExitCode' so
--   callers don't have to know the (stdout / stderr / exit status)
--   contract of each individual verb.
-- * 'parseSelfCommand' — an optparse-applicative parser the executable
--   wires into @app\/Main.hs@.
--
-- Keeping the handlers here (rather than in @app@) lets @synapse-cc@
-- reuse the same code via SELF-6 without duplicating parsing or output
-- logic.
--
-- __Verbs:__
--
-- @
-- show [--json]
-- set cookie|header \<name\> \<value-or-uri\>
-- set-from-stdin cookie|header \<name\>
-- set-secret cookie|header \<name\>            -- friendly error until SELF-8
-- unset cookie|header \<name\> [--yes] [--no-delete-from-keychain]
-- resolve cookie|header \<name\>
-- upgrade-to-keychain [cookie|header] [\<name\>]  -- friendly error until SELF-8
-- clear [--yes]
-- import-token \<path-or-->  [--to-keychain]    -- --to-keychain requires SELF-8
-- @
module Synapse.Self.Command
  ( -- * Command ADT
    SelfCommand(..)
  , SelfKind(..)
  , kindLabel
  , renderKind

    -- * Execution
  , runSelfCommand

    -- * Parser (optparse-applicative)
  , parseSelfCommand
  , selfCommandInfo

    -- * Set-value heuristic (exported for testing)
  , knownSchemes
  , wrapAsLiteralIfNeeded

    -- * JWT inspection (exported for testing)
  , JwtSummary(..)
  , JwtClaim(..)
  , decodeJwt
  , renderJwtSummary
  , renderRelativeExpiry
  ) where

import Control.Exception (IOException)
import qualified Control.Exception as E
import Data.Aeson (Value(..), (.=), object, eitherDecodeStrict')
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), NumberFormat(..), encodePretty')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Options.Applicative
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hFlush, isEOF, stderr, stdout)

import Synapse.Self
  ( CredentialRef(..)
  , ParsedUri(..)
  , ParsedUriBody(..)
  , ResolveError(..)
  , ResolverRegistry
  , StoredDefaults(..)
  , defaultRegistry
  , defaultsPath
  , emptyStoredDefaults
  , loadDefaults
  , parseUri
  , resolveRef
  , writeDefaults
  )
import qualified Synapse.Self.IO as SelfIO

-- ============================================================================
-- ADT
-- ============================================================================

-- | Cookies vs. headers. The defaults file carries both; every verb
-- except @clear@ / @show@ / @import-token@ / bare @upgrade-to-keychain@
-- requires a kind.
data SelfKind = Cookie | Header
  deriving (Show, Eq)

-- | The 'StoredDefaults' field name for a kind — @"cookies"@ or @"headers"@.
kindLabel :: SelfKind -> Text
kindLabel Cookie = "cookies"
kindLabel Header = "headers"

-- | Singular noun for messages.
renderKind :: SelfKind -> Text
renderKind Cookie = "cookie"
renderKind Header = "header"

-- | Every verb in the @_self@ subcommand tree. Each value fully
-- determines the resulting 'IO' behavior — 'runSelfCommand' is a pure
-- dispatcher over this type.
data SelfCommand
  = CmdShow         !Text !Bool
    -- ^ @show [--json]@.  Backend, jsonMode.
  | CmdSet          !Text !SelfKind !Text !Text
    -- ^ @set cookie|header \<name\> \<value-or-uri\>@.
    --   The value goes through 'wrapAsLiteralIfNeeded' before storage.
  | CmdSetFromStdin !Text !SelfKind !Text
    -- ^ @set-from-stdin cookie|header \<name\>@. Reads full stdin,
    --   stores as @literal:\<stdin\>@ regardless of scheme prefix.
  | CmdSetSecret    !Text !SelfKind !Text
    -- ^ @set-secret cookie|header \<name\>@. Requires SELF-8; emits a
    --   friendly error until then.
  | CmdUnset        !Text !SelfKind !Text !Bool !Bool
    -- ^ @unset cookie|header \<name\>@. Flags: @--yes@,
    --   @--no-delete-from-keychain@.
  | CmdResolve      !Text !SelfKind !Text
    -- ^ @resolve cookie|header \<name\>@. Prints resolved value to
    --   stdout; errors to stderr; exits non-zero on failure.
  | CmdUpgrade      !Text !(Maybe SelfKind) !(Maybe Text)
    -- ^ @upgrade-to-keychain [cookie|header] [\<name\>]@. Friendly error
    --   until SELF-8 regardless of arguments.
  | CmdClear        !Text !Bool
    -- ^ @clear [--yes]@. Deletes the backend\'s defaults.json (not the
    --   directory).
  | CmdImportToken  !Text !Text !Bool
    -- ^ @import-token \<path-or-->  [--to-keychain]@. Path @"-"@ = stdin.
    --   @--to-keychain@ requires SELF-8.
  deriving (Show, Eq)

-- ============================================================================
-- Known schemes (static; see SELF-4 ticket)
-- ============================================================================

-- | The set of scheme prefixes a @set\<value-or-uri\>@ argument can
-- carry to skip the literal wrapper. Static on purpose: this must not
-- depend on the runtime registry because SELF-8 (keychain) wires in
-- its resolver dynamically, and SELF-4 predates that.
--
-- Bare strings matching none of these get wrapped as @literal:\<value\>@.
knownSchemes :: [Text]
knownSchemes =
  [ "literal:"
  , "env://"
  , "file://"
  , "keychain://"
  , "vault://"
  , "aws-secrets://"
  , "gcp-secret://"
  ]

-- | Wrap a value as @literal:\<value\>@ unless it begins with one of
-- the 'knownSchemes'. Preserves leading\/trailing whitespace so pasted
-- tokens round-trip byte-for-byte through @set@ / @resolve@.
wrapAsLiteralIfNeeded :: Text -> Text
wrapAsLiteralIfNeeded v
  | any (`T.isPrefixOf` v) knownSchemes = v
  | otherwise                           = "literal:" <> v

-- ============================================================================
-- Parser
-- ============================================================================

-- | Full @ParserInfo@ for @_self@. The executable attaches this under
-- its top-level @_self@ subcommand; @synapse-cc@ does the same.
selfCommandInfo :: ParserInfo SelfCommand
selfCommandInfo = info (parseSelfCommand <**> helper)
  ( fullDesc
 <> progDesc "Manage per-backend defaults (cookies, headers, credentials)"
 <> header   "synapse _self - backend credential management"
  )

-- | The @\<backend\> \<verb\> ...@ parser. Exposed separately so the
-- @_self@ level can be folded under a parent @synapse ...@ parser
-- without double-@helper@.
parseSelfCommand :: Parser SelfCommand
parseSelfCommand =
  mkCmd <$> backendArg <*> verbSub
  where
    mkCmd b f = f b

    backendArg = argument (T.pack <$> str)
      ( metavar "BACKEND"
     <> help "Backend name (directory under ~/.plexus/)"
      )

    verbSub :: Parser (Text -> SelfCommand)
    verbSub = hsubparser
      ( command "show"          (info showCmd         (progDesc "Show effective defaults with resolved values"))
     <> command "set"           (info setCmd          (progDesc "Set a cookie or header to a value or URI"))
     <> command "set-from-stdin" (info setFromStdinCmd (progDesc "Set a cookie or header from stdin (always literal:)"))
     <> command "set-secret"    (info setSecretCmd    (progDesc "Push stdin to keychain and store a keychain:// ref"))
     <> command "unset"         (info unsetCmd        (progDesc "Remove a cookie or header entry"))
     <> command "resolve"       (info resolveCmd      (progDesc "Resolve and print a cookie or header value"))
     <> command "upgrade-to-keychain" (info upgradeCmd (progDesc "Move literal: refs into the keychain"))
     <> command "clear"         (info clearCmd        (progDesc "Delete this backend's defaults.json"))
     <> command "import-token"  (info importTokenCmd  (progDesc "Import a JWT from file or stdin as access_token"))
      )

    showCmd = (\j b -> CmdShow b j)
      <$> switch (long "json" <> help "Emit JSON (machine-readable)")
    setCmd = (\k n v b -> CmdSet b k n v)
      <$> kindArg
      <*> nameArg
      <*> argument (T.pack <$> str) (metavar "VALUE-OR-URI" <> help "Raw value, or a scheme-prefixed URI (literal:, env://, file://, keychain://, vault://)")
    setFromStdinCmd = (\k n b -> CmdSetFromStdin b k n) <$> kindArg <*> nameArg
    setSecretCmd    = (\k n b -> CmdSetSecret b k n)    <$> kindArg <*> nameArg
    unsetCmd = (\k n y nk b -> CmdUnset b k n y nk)
      <$> kindArg
      <*> nameArg
      <*> switch (long "yes" <> short 'y' <> help "Skip confirmation prompts")
      <*> switch (long "no-delete-from-keychain" <> help "If ref was keychain://, do not attempt to delete the keychain item")
    resolveCmd = (\k n b -> CmdResolve b k n) <$> kindArg <*> nameArg
    upgradeCmd = (\k n b -> CmdUpgrade b k n)
      <$> optional kindArg
      <*> optional nameArg
    clearCmd = (\y b -> CmdClear b y)
      <$> switch (long "yes" <> short 'y' <> help "Skip confirmation")
    importTokenCmd = (\p kc b -> CmdImportToken b p kc)
      <$> argument (T.pack <$> str) (metavar "PATH-OR--" <> help "Path to a JWT file; \"-\" reads from stdin")
      <*> switch (long "to-keychain" <> help "Push token to keychain and store a keychain:// ref (requires SELF-8)")

    kindArg = argument kindReader (metavar "KIND" <> help "cookie | header")
    nameArg = argument (T.pack <$> str) (metavar "NAME" <> help "Cookie or header name")

    kindReader = eitherReader $ \case
      "cookie"  -> Right Cookie
      "cookies" -> Right Cookie
      "header"  -> Right Header
      "headers" -> Right Header
      s         -> Left ("expected 'cookie' or 'header', got: " <> s)

-- ============================================================================
-- Execution
-- ============================================================================

-- | Run a parsed 'SelfCommand'. Returns an 'ExitCode' rather than
-- calling 'exitWith' so callers can compose.
runSelfCommand :: SelfCommand -> IO ExitCode
runSelfCommand = \case
  CmdShow b jsonMode            -> runShow b jsonMode
  CmdSet b k n v                -> runSet b k n (wrapAsLiteralIfNeeded v)
  CmdSetFromStdin b k n         -> runSetFromStdin b k n
  CmdSetSecret _ _ _            -> keychainUnavailable "set-secret"
  CmdUnset b k n yes noKeychain -> runUnset b k n yes noKeychain
  CmdResolve b k n              -> runResolve b k n
  CmdUpgrade _ _ _              -> keychainUnavailable "upgrade-to-keychain"
  CmdClear b yes                -> runClear b yes
  CmdImportToken b path toKc    -> runImportToken b path toKc

-- ---------------------------------------------------------------------------
-- Helpers shared by verbs
-- ---------------------------------------------------------------------------

keychainUnavailable :: Text -> IO ExitCode
keychainUnavailable verb = do
  TIO.hPutStrLn stderr $ T.concat
    [ "Error: `", verb, "` requires the keychain resolver, which is not "
    , "available in this build.\n"
    , "\n"
    , "The keychain integration lands with SELF-8. In the meantime:\n"
    , "  - Use `set` with an explicit scheme (env://, file://, literal:) to avoid keychain.\n"
    , "  - Or store the value in your OS keychain by hand and reference it\n"
    , "    directly with `keychain://...` once SELF-8 ships.\n"
    ]
  pure (ExitFailure 2)

-- | Pretty-print a 'ResolveError' for stderr, reusing the CLI's style
-- but slightly more verbose (names the failing URI for every error
-- variant).
renderResolveErrorText :: ResolveError -> Text
renderResolveErrorText = \case
  ResolveUnknownScheme s ->
    "unknown credential scheme `" <> s
    <> "` (no resolver registered; built-in schemes: literal, env, file)"
  ResolveNotFound (CredentialRef uri) ->
    "credential not found: " <> uri
  ResolveBackendError (CredentialRef uri) msg ->
    "credential backend error for " <> uri <> ": " <> msg
  ResolveParseError msg ->
    "malformed credential reference: " <> msg

-- | Loud load: either return the 'StoredDefaults', or print the IO
-- exception and exit. Used by every verb that reads the file.
loadOrExit :: Text -> IO StoredDefaults
loadOrExit backend = do
  r <- E.try (loadDefaults backend) :: IO (Either E.IOException StoredDefaults)
  case r of
    Right sd -> pure sd
    Left e -> do
      TIO.hPutStrLn stderr $
        "Error: failed to load defaults for backend `" <> backend <> "`: "
        <> T.pack (show e)
      _ <- E.throwIO e
      pure emptyStoredDefaults  -- unreachable; throwIO terminates

-- | Pull the map a kind refers to out of a 'StoredDefaults'.
kindMap :: SelfKind -> StoredDefaults -> Map Text CredentialRef
kindMap Cookie = sdCookies
kindMap Header = sdHeaders

-- | Replace a kind's map inside a 'StoredDefaults'.
setKindMap :: SelfKind -> Map Text CredentialRef -> StoredDefaults -> StoredDefaults
setKindMap Cookie m sd = sd { sdCookies = m }
setKindMap Header m sd = sd { sdHeaders = m }

-- ---------------------------------------------------------------------------
-- show
-- ---------------------------------------------------------------------------

-- | Entry for `_self <backend> show [--json]`. Loads, resolves each
-- entry against 'defaultRegistry', then emits either a human-readable
-- table or a JSON blob.
runShow :: Text -> Bool -> IO ExitCode
runShow backend jsonMode = do
  sd <- loadOrExit backend
  now <- getCurrentTime
  entries <- buildShowEntries defaultRegistry sd
  if jsonMode
    then do
      LBS.putStr (encodePrettyJson (entriesToJson backend entries))
      BS8.putStrLn ""
      pure ExitSuccess
    else do
      renderShowText backend sd now entries
      pure ExitSuccess

-- | An entry in the `show` output — one cookie or header.
data ShowEntry = ShowEntry
  { seKind     :: !SelfKind
  , seName     :: !Text
  , seRef      :: !CredentialRef
  , seResolver :: !Text
  , seResult   :: !(Either ResolveError Text)
  }

-- | Best-effort resolver name for a URI. Falls back to the raw scheme
-- string on unparseable URIs (which will also surface as a resolve
-- error later; this only feeds the per-line resolver label).
resolverName :: CredentialRef -> Text
resolverName ref = case parseUri ref of
  Left _  -> "?"
  Right p -> puScheme p

buildShowEntries :: ResolverRegistry -> StoredDefaults -> IO [ShowEntry]
buildShowEntries reg sd = do
  let pull k = [ (kn, cref) | (kn, cref) <- Map.toAscList (kindMap k sd) ]
  let allRefs = [(Cookie, n, r) | (n, r) <- pull Cookie]
             ++ [(Header, n, r) | (n, r) <- pull Header]
  mapM (\(k, n, r) -> do
          res <- resolveRef reg r
          pure ShowEntry
            { seKind = k
            , seName = n
            , seRef = r
            , seResolver = resolverName r
            , seResult = res
            }) allRefs

renderShowText :: Text -> StoredDefaults -> UTCTime -> [ShowEntry] -> IO ()
renderShowText backend _sd now entries = do
  let path = defaultsPath backend
  TIO.putStrLn $ "Backend: " <> backend
  TIO.putStrLn $ "Path:    " <> T.pack path
  let (cookies, headers) = partitionKinds entries
  renderSection "Cookies" cookies now
  renderSection "Headers" headers now
  case (null cookies, null headers) of
    (True, True) -> TIO.putStrLn "\n(no defaults configured for this backend)"
    _            -> pure ()

partitionKinds :: [ShowEntry] -> ([ShowEntry], [ShowEntry])
partitionKinds = foldr step ([], [])
  where
    step e (cs, hs) = case seKind e of
      Cookie -> (e : cs, hs)
      Header -> (cs, e : hs)

renderSection :: Text -> [ShowEntry] -> UTCTime -> IO ()
renderSection _ [] _ = pure ()
renderSection title es now = do
  TIO.putStrLn ""
  TIO.putStrLn $ title <> ":"
  mapM_ (renderShowEntry now) es

renderShowEntry :: UTCTime -> ShowEntry -> IO ()
renderShowEntry now ShowEntry{..} = do
  let CredentialRef uri = seRef
  TIO.putStrLn $ "  " <> seName <> " -> " <> uri <> "  [" <> seResolver <> "]"
  case seResult of
    Left err -> do
      TIO.putStrLn $ "      x " <> renderResolveErrorText err
    Right val -> do
      TIO.putStrLn "      ok resolved"
      case decodeJwt val of
        Nothing -> pure ()  -- Not a JWT; don't print the value (might be a secret).
        Just summary -> mapM_ (TIO.putStrLn . ("      " <>)) (renderJwtSummary now summary)

-- | Machine-readable JSON form of the full show output.
entriesToJson :: Text -> [ShowEntry] -> Value
entriesToJson backend entries =
  let (cookies, headers) = partitionKinds entries
  in object
    [ "backend" .= backend
    , "path"    .= defaultsPath backend
    , "cookies" .= fmap entryToJson cookies
    , "headers" .= fmap entryToJson headers
    ]

entryToJson :: ShowEntry -> Value
entryToJson ShowEntry{..} =
  let CredentialRef uri = seRef
      base =
        [ "name"     .= seName
        , "uri"      .= uri
        , "resolver" .= seResolver
        ]
      outcome = case seResult of
        Left err ->
          [ "resolved" .= False
          , "error"    .= renderResolveErrorText err
          ]
        Right val ->
          let jwt = decodeJwt val
              jwtPairs = case jwt of
                Nothing -> []
                Just s  -> [ "jwt" .= jwtToJson s ]
          in [ "resolved" .= True
             -- NEVER include the resolved value in JSON either — JWT
             -- claims go through jwtToJson (sans signature) and other
             -- secrets stay on disk.
             ] <> jwtPairs
  in object (base <> outcome)

encodePrettyJson :: Value -> LBS.ByteString
encodePrettyJson = encodePretty' Config
  { confIndent          = Spaces 2
  , confCompare         = compare
  , confNumFormat       = Generic
  , confTrailingNewline = False
  }

-- ---------------------------------------------------------------------------
-- set / set-from-stdin
-- ---------------------------------------------------------------------------

-- | Implement @set@ (after the heuristic has already been applied by
-- 'runSelfCommand').
runSet :: Text -> SelfKind -> Text -> Text -> IO ExitCode
runSet backend k name rawRef = do
  sd <- loadOrExit backend
  let m  = kindMap k sd
      m' = Map.insert name (CredentialRef rawRef) m
  writeDefaults backend (setKindMap k m' sd)
  TIO.putStrLn $ "Stored " <> renderKind k <> " `" <> name
    <> "` -> " <> rawRef <> " for backend `" <> backend <> "`."
  pure ExitSuccess

-- | Implement @set-from-stdin@: read entire stdin, wrap as @literal:@
-- regardless of content, then delegate to 'runSet'.
--
-- A single trailing newline is stripped — users invoking this via
-- @echo value | ...@ almost always mean "the value is @value@, not
-- @value\\n@." Mirrors the file resolver's newline handling
-- ('Synapse.Self.Resolve.File') for consistency.
runSetFromStdin :: Text -> SelfKind -> Text -> IO ExitCode
runSetFromStdin backend k name = do
  value <- T.pack <$> getContents
  let trimmed = stripTrailingNewline value
  runSet backend k name ("literal:" <> trimmed)

stripTrailingNewline :: Text -> Text
stripTrailingNewline t = case T.unsnoc t of
  Just (pfx, '\n') -> pfx
  _                -> t

-- ---------------------------------------------------------------------------
-- unset
-- ---------------------------------------------------------------------------

-- | Implement @unset@. If the removed ref had a @keychain://@ scheme
-- and the user doesn't opt out, prompt to "also delete from keychain"
-- — but the actual deletion is a no-op until SELF-8 lands.
runUnset :: Text -> SelfKind -> Text -> Bool -> Bool -> IO ExitCode
runUnset backend k name yes noKeychain = do
  sd <- loadOrExit backend
  let m = kindMap k sd
  case Map.lookup name m of
    Nothing -> do
      TIO.hPutStrLn stderr $
        "No " <> renderKind k <> " named `" <> name
        <> "` in backend `" <> backend <> "`."
      pure (ExitFailure 1)
    Just ref@(CredentialRef uri) -> do
      let m' = Map.delete name m
      writeDefaults backend (setKindMap k m' sd)
      TIO.putStrLn $ "Removed " <> renderKind k <> " `" <> name
        <> "` (was " <> uri <> ")."
      -- Keychain cleanup prompt (no-op until SELF-8; prompt still fires
      -- so users see consistent behavior across versions).
      let isKeychainRef = case parseUri ref of
            Right (ParsedUri "keychain" _) -> True
            _                              -> False
      if isKeychainRef && not noKeychain
        then do
          shouldDelete <-
            if yes
              then pure True
              else promptYesNo "Also delete keychain item? [Y/n] " True
          if shouldDelete
            then TIO.putStrLn $ "(keychain deletion skipped: SELF-8 pending; "
                 <> "please remove the keychain entry manually for now.)"
            else TIO.putStrLn "(kept the keychain entry on disk)"
        else pure ()
      pure ExitSuccess

-- | Prompt yes/no. Default used when the user hits Enter.
promptYesNo :: Text -> Bool -> IO Bool
promptYesNo prompt defaultYes = do
  TIO.putStr prompt
  hFlush stdout
  done <- isEOF
  if done
    then pure defaultYes
    else do
      line <- TIO.getLine
      let trimmed = T.toLower (T.strip line)
      pure $ case trimmed of
        ""  -> defaultYes
        "y" -> True
        "yes" -> True
        "n" -> False
        "no" -> False
        _   -> defaultYes

-- ---------------------------------------------------------------------------
-- resolve
-- ---------------------------------------------------------------------------

-- | Implement @resolve@. Prints the URI to stdout (first line) and the
-- resolved value (second line) on success. On failure, the URI still
-- goes to stdout (useful for diagnostics) and the error goes to
-- stderr, with a non-zero exit.
--
-- Unlike @show@, @resolve@ *does* print the resolved value — this is
-- the command users run when they explicitly want to see the secret
-- (e.g. for comparison, or to pipe into another tool).
runResolve :: Text -> SelfKind -> Text -> IO ExitCode
runResolve backend k name = do
  sd <- loadOrExit backend
  let m = kindMap k sd
  case Map.lookup name m of
    Nothing -> do
      TIO.hPutStrLn stderr $
        "No " <> renderKind k <> " named `" <> name
        <> "` in backend `" <> backend <> "`."
      pure (ExitFailure 1)
    Just ref@(CredentialRef uri) -> do
      TIO.putStrLn uri
      r <- resolveRef defaultRegistry ref
      case r of
        Right val -> do
          TIO.putStrLn val
          pure ExitSuccess
        Left err -> do
          TIO.hPutStrLn stderr $ "Error: " <> renderResolveErrorText err
          pure (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- clear
-- ---------------------------------------------------------------------------

-- | Implement @clear@. Prompts unless @--yes@. Deletes the defaults
-- file only; never removes the parent @~\/.plexus\/\<backend\>@
-- directory (might hold unrelated files).
runClear :: Text -> Bool -> IO ExitCode
runClear backend yes = do
  path <- SelfIO.absDefaultsPath backend
  exists <- doesFileExist path
  if not exists
    then do
      TIO.putStrLn $ "No defaults file to clear for backend `" <> backend
        <> "` (path: " <> T.pack path <> ")."
      pure ExitSuccess
    else do
      proceed <-
        if yes
          then pure True
          else promptYesNo
            ("Delete " <> T.pack path <> "? [y/N] ") False
      if proceed
        then do
          r <- E.try (removeFile path) :: IO (Either IOException ())
          case r of
            Right () -> do
              TIO.putStrLn $ "Cleared defaults for backend `" <> backend <> "`."
              pure ExitSuccess
            Left e -> do
              TIO.hPutStrLn stderr $ "Error: " <> T.pack (show e)
              pure (ExitFailure 1)
        else do
          TIO.putStrLn "Aborted."
          pure (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- import-token
-- ---------------------------------------------------------------------------

-- | Implement @import-token@. Reads a file (or stdin if @"-"@), stores
-- the result as @literal:\<contents\>@ under @cookies.access_token@.
--
-- @--to-keychain@ is gated on SELF-8 and errors out until then.
runImportToken :: Text -> Text -> Bool -> IO ExitCode
runImportToken backend path toKeychain
  | toKeychain = keychainUnavailable "import-token --to-keychain"
  | otherwise  = do
      token <- readTokenSource path
      let trimmed = T.strip token
      if T.null trimmed
        then do
          TIO.hPutStrLn stderr "Error: token source is empty."
          pure (ExitFailure 1)
        else do
          sd <- loadOrExit backend
          let m  = sdCookies sd
              m' = Map.insert "access_token"
                     (CredentialRef ("literal:" <> trimmed)) m
          writeDefaults backend (sd { sdCookies = m' })
          TIO.putStrLn $ "Imported access_token for backend `" <> backend
            <> "` (stored as literal:). "
            <> "Consider `upgrade-to-keychain` once SELF-8 lands for a more secure posture."
          pure ExitSuccess

-- | Read a token from a path. @"-"@ means stdin; all other strings are
-- paths on disk. A trailing newline is preserved here — the caller
-- trims before storage.
readTokenSource :: Text -> IO Text
readTokenSource "-" = T.pack <$> getContents
readTokenSource p   = TIO.readFile (T.unpack p)

-- ============================================================================
-- JWT inspection
-- ============================================================================

-- | Structured summary of a JWT. We only record fields the CLI prints.
-- Signatures are never stored.
data JwtSummary = JwtSummary
  { jsAlg      :: !(Maybe Text)
  , jsKid      :: !(Maybe Text)
  , jsIss      :: !(Maybe Text)
  , jsAud      :: !(Maybe Text)  -- ^ Rendered: first aud entry if array.
  , jsSub      :: !(Maybe Text)
  , jsExp      :: !(Maybe UTCTime)
  , jsIat      :: !(Maybe UTCTime)
  , jsUsername :: !(Maybe Text)  -- ^ @preferred_username@
  , jsEmail    :: !(Maybe Text)
  , jsExtraClaims :: ![JwtClaim]  -- ^ Retained for future; unused in v1.
  }
  deriving (Show, Eq)

-- | A single JWT claim, name + rendered value. Reserved for future
-- extension (e.g. scope, roles). Currently unused — the named fields
-- on 'JwtSummary' cover everything we print.
data JwtClaim = JwtClaim
  { jcName :: !Text
  , jcValue :: !Text
  }
  deriving (Show, Eq)

-- | Attempt to decode a string as a JWT. Returns 'Nothing' on anything
-- that isn't JWT-shaped. A "JWT-shaped" value has exactly two dots,
-- each of the three segments base64-decodes cleanly (URL-safe, with or
-- without padding), and the header JSON has an @alg@ field.
--
-- Decoding never fails loudly: a value that looks JWT-shaped but has
-- a non-JSON payload yields 'Nothing'.
decodeJwt :: Text -> Maybe JwtSummary
decodeJwt t = case T.splitOn "." t of
  [h, p, _sig]
    | not (T.null h), not (T.null p) -> do
        headerJson <- decodeSegment h
        payloadJson <- decodeSegment p
        -- Header must have an `alg` field to count as a JWT.
        case headerJson of
          Object o | KM.member "alg" o -> Just (buildSummary headerJson payloadJson)
          _                            -> Nothing
  _ -> Nothing

decodeSegment :: Text -> Maybe Value
decodeSegment seg =
  let bs = TE.encodeUtf8 seg
      -- JWT uses URL-safe base64 with padding stripped.
      padded = padBase64 bs
  in case B64U.decode padded of
       Left _    -> Nothing
       Right raw -> case eitherDecodeStrict' raw :: Either String Value of
         Left _  -> Nothing
         Right v -> Just v

-- | Re-pad a URL-safe base64 string to a multiple of 4. @base64-bytestring@
-- requires padding for decoding; JWTs strip it.
padBase64 :: BS.ByteString -> BS.ByteString
padBase64 bs =
  let n = BS.length bs
      r = n `mod` 4
      pad = case r of
        0 -> 0
        2 -> 2
        3 -> 1
        _ -> 0  -- 1 is invalid; let decode fail on it
  in bs <> BS8.replicate pad '='

buildSummary :: Value -> Value -> JwtSummary
buildSummary header payload = JwtSummary
  { jsAlg      = stringClaim header "alg"
  , jsKid      = stringClaim header "kid"
  , jsIss      = stringClaim payload "iss"
  , jsAud      = audClaim payload
  , jsSub      = stringClaim payload "sub"
  , jsExp      = timeClaim payload "exp"
  , jsIat      = timeClaim payload "iat"
  , jsUsername = stringClaim payload "preferred_username"
  , jsEmail    = stringClaim payload "email"
  , jsExtraClaims = []
  }

-- | Pull a string claim out of a JSON object. Returns 'Nothing' for
-- non-object values, missing fields, or fields that aren't strings.
stringClaim :: Value -> Text -> Maybe Text
stringClaim (Object o) k = case KM.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing
stringClaim _ _ = Nothing

-- | @aud@ can be either a string or an array of strings. We render the
-- first element in either case.
audClaim :: Value -> Maybe Text
audClaim (Object o) = case KM.lookup "aud" o of
  Just (String s)   -> Just s
  Just (Array arr)  -> case foldMap stringOf arr of
    []    -> Nothing
    (x:_) -> Just x
  _ -> Nothing
  where
    stringOf (String s) = [s]
    stringOf _          = []
audClaim _ = Nothing

-- | Integer seconds-since-epoch as a 'UTCTime'.
timeClaim :: Value -> Text -> Maybe UTCTime
timeClaim (Object o) k = case KM.lookup (Key.fromText k) o of
  Just (Number n) -> Just (posixSecondsToUTCTime (realToFrac n))
  _               -> Nothing
timeClaim _ _ = Nothing

-- | Render a JWT summary as a list of aligned label/value lines.
-- Never emits the signature; never emits raw claim bodies.
renderJwtSummary :: UTCTime -> JwtSummary -> [Text]
renderJwtSummary now JwtSummary{..} = catMaybes
  [ fmap (labelled "alg") jsAlg
  , fmap (labelled "kid" . truncateKid) jsKid
  , fmap (labelled "iss") jsIss
  , fmap (labelled "aud") jsAud
  , fmap (labelled "sub") jsSub
  , fmap (\t ->
      labelled "exp"
        (T.pack (formatISO8601 t) <> "  (" <> renderRelativeExpiry now t <> ")"))
      jsExp
  , fmap (\t -> labelled "iat" (T.pack (formatISO8601 t))) jsIat
  , case (jsUsername, jsEmail) of
      (Just u, Just e) -> Just $ labelled "user" (u <> " (" <> e <> ")")
      (Just u, Nothing) -> Just $ labelled "user" u
      (Nothing, Just e) -> Just $ labelled "user" e
      (Nothing, Nothing) -> Nothing
  ]
  where
    labelled label val = label <> padTo 5 label <> val
    padTo n lab = T.replicate (max 1 (n - T.length lab)) " "

-- | Long `kid` values are typically random key-IDs that add visual
-- noise without aiding identification. Keep the first 6 chars and
-- elide the tail.
truncateKid :: Text -> Text
truncateKid t
  | T.length t > 16 = T.take 6 t <> "..." <> T.takeEnd 4 t
  | otherwise       = t

formatISO8601 :: UTCTime -> String
formatISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Render @exp - now@ as a human-readable relative duration:
-- "expired 16d 4h ago" for past, "valid for 4m 32s" for future.
--
-- Uses the two largest non-zero units (d/h, h/m, m/s, or just s for
-- sub-minute), which matches the operator-facing prose the ticket asks
-- for: glance-readable, enough precision to act on.
renderRelativeExpiry :: UTCTime -> UTCTime -> Text
renderRelativeExpiry now exp_ =
  let delta = diffUTCTime exp_ now
      secs :: Integer
      secs = floor (realToFrac delta :: Double)
      absSecs = abs secs
      (d, r1) = absSecs `divMod` 86400
      (h, r2) = r1     `divMod` 3600
      (m, s)  = r2     `divMod` 60
      unitPair = case (d, h, m, s) of
        _ | d > 0 && h > 0 -> T.pack (show d) <> "d " <> T.pack (show h) <> "h"
        _ | d > 0          -> T.pack (show d) <> "d"
        _ | h > 0 && m > 0 -> T.pack (show h) <> "h " <> T.pack (show m) <> "m"
        _ | h > 0          -> T.pack (show h) <> "h"
        _ | m > 0 && s > 0 -> T.pack (show m) <> "m " <> T.pack (show s) <> "s"
        _ | m > 0          -> T.pack (show m) <> "m"
        _                  -> T.pack (show s) <> "s"
  in if secs < 0
       then "expired " <> unitPair <> " ago"
       else "valid for " <> unitPair

-- | Minimal JSON render of a 'JwtSummary' for the @--json@ output path.
jwtToJson :: JwtSummary -> Value
jwtToJson JwtSummary{..} = object $ catMaybes
  [ fmap ("alg" .=) jsAlg
  , fmap ("kid" .=) jsKid
  , fmap ("iss" .=) jsIss
  , fmap ("aud" .=) jsAud
  , fmap ("sub" .=) jsSub
  , fmap (\t -> "exp" .= formatISO8601 t) jsExp
  , fmap (\t -> "iat" .= formatISO8601 t) jsIat
  , fmap ("preferred_username" .=) jsUsername
  , fmap ("email" .=) jsEmail
  ]
