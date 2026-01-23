{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Template generation for Plexus methods
--
-- Generates example CLI invocations with realistic parameter values
-- to help users understand how to use commands.
module Synapse.Self.Template
  ( handleTemplate
  , LimitOptions(..)
  , parseLimitOptions
  , applyLimit
  ) where

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

import Synapse.Monad
import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.Self.Pattern
import Synapse.Self.Examples

-- ============================================================================
-- Limit/Pagination
-- ============================================================================

-- | Options for limiting output
data LimitOptions = LimitOptions
  { loLower :: Int       -- ^ Start index (0-based)
  , loUpper :: Maybe Int -- ^ End index (exclusive), Nothing = unlimited
  , loTotal :: Int       -- ^ Total to show (used when upper not set)
  }
  deriving stock (Show, Eq)

-- | Default limit: show first 10 results
defaultLimit :: LimitOptions
defaultLimit = LimitOptions 0 Nothing 10

-- | Parse limit options from CLI parameters
-- Supports:
--   --limit N : Show first N results
--   --limit.lower L --limit.upper U : Show results from L to U
parseLimitOptions :: [(Text, Text)] -> LimitOptions
parseLimitOptions params =
  let findInt key = lookup key params >>= readMaybe . T.unpack
      limitVal = findInt "limit"
      lowerVal = findInt "limit.lower"
      upperVal = findInt "limit.upper"
  in case (limitVal, lowerVal, upperVal) of
      (Just n, Nothing, Nothing) -> LimitOptions 0 (Just n) n
      (Nothing, Just l, Just u) -> LimitOptions l (Just u) (u - l)
      (Nothing, Just l, Nothing) -> LimitOptions l Nothing 10
      _ -> defaultLimit

-- | Apply limit to a list of methods
applyLimit :: LimitOptions -> [MethodDef] -> [MethodDef]
applyLimit LimitOptions{..} methods =
  let afterDrop = drop loLower methods
      limited = case loUpper of
        Just u -> take (u - loLower) afterDrop
        Nothing -> take loTotal afterDrop
  in limited

-- ============================================================================
-- Template Generation
-- ============================================================================

-- | Handle the _self template command
handleTemplate :: [Text] -> [(Text, Text)] -> SynapseM ()
handleTemplate rest params = do
  -- Parse pattern (default to *.* if missing)
  -- Note: rest comes from parsePathAndParams which splits on dots,
  -- so we need to rejoin to get the full pattern
  let patternText = case rest of
        [] -> "*.*"
        segs -> T.intercalate "." segs

  -- Compile pattern
  pattern <- case parsePattern patternText of
    Left err -> throwParse err
    Right p -> pure p

  -- Parse limit options
  let limitOpts = parseLimitOptions params

  -- Build full IR from root
  ir <- buildIR []

  -- Get all methods and filter by pattern
  let allMethods = Map.elems (irMethods ir)
  let matches = matchMethods pattern allMethods

  -- Check for no matches
  when (null matches) $
    throwParse $ "No methods match pattern: " <> patternText

  -- Apply limit/pagination
  let bounded = applyLimit limitOpts matches

  -- Display templates
  liftIO $ displayTemplates ir bounded (length matches) limitOpts

-- | Display formatted templates for methods
displayTemplates :: IR -> [MethodDef] -> Int -> LimitOptions -> IO ()
displayTemplates ir methods totalCount opts = do
  let shownCount = length methods

  -- Header showing count
  TIO.putStrLn $ "Showing " <> T.pack (show shownCount)
               <> " of " <> T.pack (show totalCount) <> " matches"
  TIO.putStrLn ""

  -- Render each method template
  forM_ methods $ \method -> do
    TIO.putStrLn $ T.replicate 60 "━"
    TIO.putStrLn $ mdFullPath method
    TIO.putStrLn $ T.replicate 60 "━"
    TIO.putStrLn ""

    -- Description
    case mdDescription method of
      Just desc -> do
        TIO.putStrLn "Description:"
        TIO.putStrLn $ "  " <> desc
        TIO.putStrLn ""
      Nothing -> pure ()

    -- Template command
    let templateCmd = generateTemplateCommand ir method
    TIO.putStrLn "Template:"
    TIO.putStrLn $ "  " <> templateCmd
    TIO.putStrLn ""

    -- Parameter documentation
    displayParams method
    TIO.putStrLn ""

  -- Truncation warning if we're showing less than total
  when (totalCount > shownCount) $
    TIO.putStrLn $ "\n⚠ Showing " <> T.pack (show shownCount)
                 <> " of " <> T.pack (show totalCount)
                 <> " matches. Use --limit to see more."

-- | Generate a template command line for a method
generateTemplateCommand :: IR -> MethodDef -> Text
generateTemplateCommand ir MethodDef{..} =
  let -- Convert namespace.method to CLI path
      -- e.g., "cone.chat" -> "synapse plexus cone chat"
      basePath = "synapse plexus " <> T.replace "." " " mdNamespace <> " " <> mdName

      -- Generate parameter flags
      paramFlags = map (generateExampleParam ir) mdParams

      -- Combine with line continuation
      allParts = basePath : paramFlags
  in if null paramFlags
     then basePath
     else T.intercalate " \\\n  " allParts

-- | Display parameter documentation
displayParams :: MethodDef -> IO ()
displayParams MethodDef{..} = do
  let required = filter pdRequired mdParams
  let optional = filter (not . pdRequired) mdParams

  unless (null required) $ do
    TIO.putStrLn "Required Parameters:"
    forM_ required $ \p -> do
      let desc = case pdDescription p of
            Just d -> d
            Nothing -> "(no description)"
      TIO.putStrLn $ "  • " <> pdName p <> ": " <> desc
    TIO.putStrLn ""

  unless (null optional) $ do
    TIO.putStrLn "Optional Parameters:"
    forM_ optional $ \p -> do
      let desc = case pdDescription p of
            Just d -> d
            Nothing -> "(no description)"
      TIO.putStrLn $ "  • " <> pdName p <> ": " <> desc
