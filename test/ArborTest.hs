#!/usr/bin/env cabal
{- cabal:
build-depends: base, symbols, text, streaming
-}

-- | Integration test for Arbor operations
-- Run with: cabal run symbols-cli -- test
-- Or: runhaskell test/ArborTest.hs

module Main where

import Plexus (connect, disconnect, defaultConfig)
import qualified Activation.Arbor as Arbor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Streaming.Prelude as S
import Control.Monad (void)

main :: IO ()
main = do
  putStrLn "=== Arbor Integration Tests ==="
  putStrLn ""

  conn <- connect defaultConfig

  -- Test 1: Create tree
  putStrLn "1. Creating tree..."
  treeId <- createTree conn "test-suite"
  putStrLn $ "   Tree ID: " <> T.unpack treeId

  -- Test 2: Get tree skeleton to find root
  putStrLn ""
  putStrLn "2. Getting tree skeleton..."
  rootId <- getRoot conn treeId
  putStrLn $ "   Root ID: " <> T.unpack rootId

  -- Test 3: Create child nodes
  putStrLn ""
  putStrLn "3. Creating child nodes of root..."
  node1 <- createChildNode conn treeId rootId "User: Hello, how are you?"
  putStrLn $ "   Node 1: " <> T.unpack node1

  node2 <- createChildNode conn treeId rootId "User: What's the weather?"
  putStrLn $ "   Node 2: " <> T.unpack node2

  -- Test 4: Create grandchild
  putStrLn ""
  putStrLn "4. Creating grandchild node..."
  node1a <- createChildNode conn treeId node1 "Assistant: I'm doing well, thanks!"
  putStrLn $ "   Node 1a: " <> T.unpack node1a

  node1b <- createChildNode conn treeId node1 "Assistant: I'm great! How about you?"
  putStrLn $ "   Node 1b: " <> T.unpack node1b

  -- Test 5: Create great-grandchild
  putStrLn ""
  putStrLn "5. Creating great-grandchild..."
  node1a1 <- createChildNode conn treeId node1a "User: Glad to hear it!"
  putStrLn $ "   Node 1a1: " <> T.unpack node1a1

  -- Test 6: List trees
  putStrLn ""
  putStrLn "6. Listing all trees..."
  listTrees conn

  -- Test 7: Get leaves
  putStrLn ""
  putStrLn "7. Getting leaf nodes..."
  getLeaves conn treeId

  -- Test 8: Get children of root
  putStrLn ""
  putStrLn "8. Getting children of root..."
  getChildren conn treeId rootId

  -- Test 9: Get path to deepest node
  putStrLn ""
  putStrLn "9. Getting path to deepest node..."
  getPath conn treeId node1a1

  -- Test 10: Render tree
  putStrLn ""
  putStrLn "10. Rendering tree:"
  putStrLn "----------------------------------------"
  renderTree conn treeId
  putStrLn "----------------------------------------"

  -- Test 11: Claim/Release
  putStrLn ""
  putStrLn "11. Testing claim/release..."
  claimTree conn treeId "test-owner"
  releaseTree conn treeId "test-owner"

  -- Done
  putStrLn ""
  putStrLn "=== All tests completed ==="

  disconnect conn

-- Helper functions

createTree :: Arbor.PlexusConnection -> Text -> IO Text
createTree conn owner = do
  result <- S.head_ $ Arbor.treeCreate conn Nothing owner
  case result of
    Just (Arbor.TreeCreated tid) -> pure tid
    _ -> error "Failed to create tree"

getRoot :: Arbor.PlexusConnection -> Text -> IO Text
getRoot conn treeId = do
  result <- S.head_ $ Arbor.treeGetSkeleton conn treeId
  case result of
    Just (Arbor.TreeSkeletonData skel) -> pure $ Arbor.treeSkeletonRoot skel
    _ -> error "Failed to get skeleton"

createChildNode :: Arbor.PlexusConnection -> Text -> Text -> Text -> IO Text
createChildNode conn treeId parentId content = do
  result <- S.head_ $ Arbor.nodeCreateText conn treeId (Just parentId) content Nothing
  case result of
    Just (Arbor.NodeCreated _ nid _) -> pure nid
    _ -> error $ "Failed to create node: " <> T.unpack content

listTrees :: Arbor.PlexusConnection -> IO ()
listTrees conn = S.mapM_ printEvent $ Arbor.treeList conn

getLeaves :: Arbor.PlexusConnection -> Text -> IO ()
getLeaves conn treeId = S.mapM_ printEvent $ Arbor.contextListLeaves conn treeId

getChildren :: Arbor.PlexusConnection -> Text -> Text -> IO ()
getChildren conn treeId nodeId = S.mapM_ printEvent $ Arbor.nodeGetChildren conn treeId nodeId

getPath :: Arbor.PlexusConnection -> Text -> Text -> IO ()
getPath conn treeId nodeId = S.mapM_ printEvent $ Arbor.contextGetPath conn treeId nodeId

renderTree :: Arbor.PlexusConnection -> Text -> IO ()
renderTree conn treeId = S.mapM_ printRender $ Arbor.treeRender conn treeId
  where
    printRender (Arbor.TreeRenderResult _ r) = TIO.putStrLn r
    printRender e = print e

claimTree :: Arbor.PlexusConnection -> Text -> Text -> IO ()
claimTree conn treeId owner = do
  putStrLn $ "   Claiming tree for " <> T.unpack owner <> "..."
  S.mapM_ printEvent $ Arbor.treeClaim conn treeId owner 1

releaseTree :: Arbor.PlexusConnection -> Text -> Text -> IO ()
releaseTree conn treeId owner = do
  putStrLn $ "   Releasing tree for " <> T.unpack owner <> "..."
  S.mapM_ printEvent $ Arbor.treeRelease conn treeId owner 1

printEvent :: Arbor.ArborEvent -> IO ()
printEvent (Arbor.TreeList tids) =
  putStrLn $ "   Trees: " <> show (length tids) <> " total"
printEvent (Arbor.ContextLeaves _ leaves) =
  putStrLn $ "   Leaves: " <> show (map T.unpack leaves)
printEvent (Arbor.NodeChildren _ _ children) =
  putStrLn $ "   Children: " <> show (map T.unpack children)
printEvent (Arbor.ContextPathData _ nodes) =
  putStrLn $ "   Path: " <> show (length nodes) <> " nodes"
printEvent (Arbor.TreeClaimed _ owner count) =
  putStrLn $ "   Claimed by " <> T.unpack owner <> " (count: " <> show count <> ")"
printEvent (Arbor.TreeReleased _ owner count) =
  putStrLn $ "   Released by " <> T.unpack owner <> " (count: " <> show count <> ")"
printEvent e = print e
