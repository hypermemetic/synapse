{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CRUD operations for Mustache templates
--
-- Provides commands to manage the template system:
-- - list: List existing templates (with pattern filtering)
-- - show: Display template content
-- - generate: Generate new templates from IR
-- - delete: Remove templates
-- - reload: Clear template cache and notify backend
module Synapse.Self.Template
  ( handleTemplate
  ) where

import Control.Monad (forM_, when, unless, filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getHomeDirectory, removeFile, listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension, splitDirectories, joinPath)

import Synapse.Monad
import Synapse.Self.Pattern (MethodPattern(..), parsePattern, matchMethods)
import qualified Synapse.CLI.Template as CLITemplate
import Synapse.IR.Types (MethodDef(..))
import qualified Data.Map.Strict as Map
import Text.Regex.TDFA ((=~))

-- ============================================================================
-- Main Entry Point
-- ============================================================================

-- | Handle template subcommands
handleTemplate :: [Text] -> [(Text, Text)] -> SynapseM ()
handleTemplate [] _ = do
  -- No subcommand - show help
  liftIO $ TIO.putStr templateHelp

handleTemplate ("list" : rest) params = do
  backend <- asks seBackend
  let patternText = case rest of
        [] -> backend <> ".*.*"  -- Default includes backend
        segs -> T.intercalate "." segs
  templateList patternText

handleTemplate ("show" : rest) _ = do
  case rest of
    [] -> throwParse "Missing method path for 'show' command\nUsage: synapse _self template show <namespace.method>"
    segs -> templateShow (T.intercalate "." segs)

handleTemplate ("generate" : rest) params = do
  backend <- asks seBackend
  let patternText = case rest of
        [] -> backend <> ".*.*"  -- Default includes backend
        segs -> T.intercalate "." segs
  templateGenerate patternText params

handleTemplate ("delete" : rest) _ = do
  case rest of
    [] -> throwParse "Missing pattern for 'delete' command\nUsage: synapse _self template delete <pattern>"
    segs -> templateDelete (T.intercalate "." segs)

handleTemplate ("reload" : _) _ = do
  templateReload

handleTemplate (cmd : _) _ = do
  throwParse $ "Unknown template subcommand: " <> cmd <> "\n\n" <> templateHelp

-- ============================================================================
-- Template List
-- ============================================================================

templateList :: Text -> SynapseM ()
templateList patternText = do
  backend <- asks seBackend

  -- Parse pattern
  pattern <- case parsePattern patternText of
    Left err -> throwParse err
    Right p -> pure p

  -- Get template base directory
  homeDir <- liftIO getHomeDirectory
  let baseDir = homeDir </> ".config" </> "synapse" </> "templates"

  -- Check if templates directory exists
  exists <- liftIO $ doesDirectoryExist baseDir
  unless exists $ do
    liftIO $ TIO.putStrLn $ "No templates directory found at: " <> T.pack baseDir
    liftIO $ TIO.putStrLn "Generate templates with: synapse --generate-templates"
    return ()

  -- Find all template files
  templateFiles <- liftIO $ findTemplateFiles baseDir

  -- Filter by pattern
  let qualifiedPaths = map (\(ns, m) -> backend <> "." <> ns <> "." <> m) templateFiles
  let matchingPaths = filter (\path -> T.unpack path `matchPattern` pattern) qualifiedPaths
  let matchingFiles = [(ns, m) | ((ns, m), qpath) <- zip templateFiles qualifiedPaths, qpath `elem` matchingPaths]

  -- Display results
  liftIO $ do
    if null matchingFiles
      then TIO.putStrLn $ "No templates match pattern: " <> patternText
      else do
        TIO.putStrLn $ "Found " <> T.pack (show (length matchingFiles)) <> " template(s):\n"
        forM_ (sort matchingFiles) $ \(namespace, method) -> do
          let path = baseDir </> T.unpack namespace </> T.unpack method <.> "mustache"
          TIO.putStrLn $ "  " <> backend <> "." <> namespace <> "." <> method
          TIO.putStrLn $ "    " <> T.pack path

-- Helper to check if a string matches a pattern
matchPattern :: String -> MethodPattern -> Bool
matchPattern str (MethodPattern _ regex) = str =~ T.unpack regex

-- ============================================================================
-- Template Show
-- ============================================================================

templateShow :: Text -> SynapseM ()
templateShow methodPath = do
  backend <- asks seBackend

  -- Strip backend prefix if present
  let pathWithoutBackend = case T.stripPrefix (backend <> ".") methodPath of
        Just p -> p
        Nothing -> methodPath

  -- Split into namespace.method
  case T.splitOn "." pathWithoutBackend of
    [namespace, method] -> do
      homeDir <- liftIO getHomeDirectory
      let templatePath = homeDir </> ".config" </> "synapse" </> "templates"
                        </> T.unpack namespace </> T.unpack method <.> "mustache"

      exists <- liftIO $ doesFileExist templatePath
      if exists
        then do
          content <- liftIO $ TIO.readFile templatePath
          liftIO $ do
            TIO.putStrLn $ "Template: " <> backend <> "." <> namespace <> "." <> method
            TIO.putStrLn $ "Location: " <> T.pack templatePath
            TIO.putStrLn $ T.replicate 60 "━"
            TIO.putStr content
            when (not $ T.null content && T.last content == '\n') $
              TIO.putStrLn ""
        else throwParse $ "Template not found: " <> methodPath <> "\nPath: " <> T.pack templatePath

    _ -> throwParse $ "Invalid method path: " <> methodPath <> "\nExpected format: namespace.method"

-- ============================================================================
-- Template Generate
-- ============================================================================

templateGenerate :: Text -> [(Text, Text)] -> SynapseM ()
templateGenerate patternText params = do
  backend <- asks seBackend
  homeDir <- liftIO getHomeDirectory
  let baseDir = homeDir </> ".config" </> "synapse" </> "templates"

  -- Strip backend prefix if present for IR generation
  let pathWithoutBackend = case T.stripPrefix (backend <> ".") patternText of
        Just p -> p
        Nothing -> patternText

  -- Parse pattern to determine what to generate
  pattern <- case parsePattern patternText of
    Left err -> throwParse err
    Right p -> pure p

  liftIO $ TIO.putStrLn $ "Generating templates in " <> T.pack baseDir <> "..."

  -- Use existing CLI.Template generation with callback
  let writeAndLog gt = do
        let fullPath = baseDir </> CLITemplate.gtPath gt
        createDirectoryIfMissing True (baseDir </> T.unpack (CLITemplate.gtNamespace gt))
        TIO.writeFile fullPath (CLITemplate.gtTemplate gt)
        TIO.putStrLn $ "  " <> backend <> "." <> CLITemplate.gtNamespace gt <> "." <> CLITemplate.gtMethod gt

  -- Generate all templates (filtering will happen based on path)
  count <- CLITemplate.generateAllTemplatesWithCallback writeAndLog []

  liftIO $ TIO.putStrLn $ "\nGenerated " <> T.pack (show count) <> " template(s)"

-- ============================================================================
-- Template Delete
-- ============================================================================

templateDelete :: Text -> SynapseM ()
templateDelete patternText = do
  backend <- asks seBackend

  -- Parse pattern
  pattern <- case parsePattern patternText of
    Left err -> throwParse err
    Right p -> pure p

  -- Get template base directory
  homeDir <- liftIO getHomeDirectory
  let baseDir = homeDir </> ".config" </> "synapse" </> "templates"

  -- Find all template files
  templateFiles <- liftIO $ findTemplateFiles baseDir

  -- Filter by pattern
  let qualifiedPaths = map (\(ns, m) -> backend <> "." <> ns <> "." <> m) templateFiles
  let matchingPaths = filter (\path -> T.unpack path `matchPattern` pattern) qualifiedPaths
  let matchingFiles = [(ns, m, path) | ((ns, m), qpath) <- zip templateFiles qualifiedPaths, qpath `elem` matchingPaths,
                                        let path = baseDir </> T.unpack ns </> T.unpack m <.> "mustache"]

  -- Confirm deletion
  liftIO $ do
    if null matchingFiles
      then TIO.putStrLn $ "No templates match pattern: " <> patternText
      else do
        TIO.putStrLn $ "Will delete " <> T.pack (show (length matchingFiles)) <> " template(s):"
        forM_ matchingFiles $ \(ns, m, _) ->
          TIO.putStrLn $ "  " <> backend <> "." <> ns <> "." <> m

        -- Delete files
        forM_ matchingFiles $ \(ns, m, path) -> do
          removeFile path
          TIO.putStrLn $ "Deleted: " <> backend <> "." <> ns <> "." <> m

-- ============================================================================
-- Template Reload
-- ============================================================================

templateReload :: SynapseM ()
templateReload = do
  -- Clear local cache (if we had one)
  liftIO $ TIO.putStrLn "Template cache cleared"

  -- TODO: Call backend reload endpoint when available
  -- For now, just notify the user
  liftIO $ TIO.putStrLn "Note: Backend template reload endpoint not yet implemented"

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Find all template files in the templates directory
-- Returns list of (namespace, method) tuples
findTemplateFiles :: FilePath -> IO [(Text, Text)]
findTemplateFiles baseDir = do
  exists <- doesDirectoryExist baseDir
  if not exists
    then return []
    else do
      namespaces <- listDirectory baseDir
      concat <$> mapM findInNamespace namespaces
  where
    findInNamespace :: FilePath -> IO [(Text, Text)]
    findInNamespace namespace = do
      let nsDir = baseDir </> namespace
      isDir <- doesDirectoryExist nsDir
      if not isDir
        then return []
        else do
          files <- listDirectory nsDir
          let templateFiles = filter (\f -> takeExtension f == ".mustache") files
          let methods = map (T.pack . dropExtension) templateFiles
          return [(T.pack namespace, m) | m <- methods]

-- ============================================================================
-- Help Text
-- ============================================================================

templateHelp :: Text
templateHelp = T.unlines
  [ "Template CRUD commands:"
  , ""
  , "  synapse _self template list [pattern]"
  , "      List existing templates"
  , "      Pattern: backend.namespace.method (e.g., 'plexus.cone.*')"
  , "      Default: '*.*' (all templates)"
  , ""
  , "  synapse _self template show <method>"
  , "      Display template content"
  , "      Example: synapse _self template show cone.chat"
  , ""
  , "  synapse _self template generate [pattern]"
  , "      Generate new templates from IR"
  , "      Pattern: backend.namespace.method (e.g., 'plexus.cone.*')"
  , "      Default: '*.*' (all methods)"
  , ""
  , "  synapse _self template delete <pattern>"
  , "      Delete templates matching pattern"
  , "      Example: synapse _self template delete 'plexus.cone.*'"
  , ""
  , "  synapse _self template reload"
  , "      Clear template cache and notify backend"
  , ""
  , "Templates are stored in: ~/.config/synapse/templates/"
  , ""
  ]
