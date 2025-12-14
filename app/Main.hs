module Main where

import Plexus (connect, disconnect, defaultConfig)
import qualified Activation.Bash as Bash
import qualified Activation.Health as Health
import qualified Activation.Arbor as Arbor
import qualified Activation.Cone as Cone
import Data.Aeson (object, (.=), Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Streaming.Prelude as S
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr)
import Options.Applicative

-- ============================================================================
-- Command Types
-- ============================================================================

data Command
  = Health
  | Bash BashCmd
  | Arbor ArborCmd
  | Cone ConeCmd
  deriving (Show)

data BashCmd
  = BashExecute [String]
  deriving (Show)

data ArborCmd
  = ArborTree ArborTreeCmd
  | ArborNode ArborNodeCmd
  | ArborContext ArborContextCmd
  deriving (Show)

data ArborTreeCmd
  = TreeList
  | TreeListScheduled
  | TreeListArchived
  | TreeCreate String
  | TreeGet String
  | TreeGetSkeleton String
  | TreeRender String
  | TreeUpdateMetadata String String
  | TreeClaim String String Int
  | TreeRelease String String Int
  deriving (Show)

data ArborNodeCmd
  = NodeCreateText String String
  | NodeCreateTextChild String String String
  | NodeGet String String
  | NodeChildren String String
  | NodeParent String String
  | NodePath String String
  deriving (Show)

data ArborContextCmd
  = ContextLeaves String
  | ContextPath String String
  | ContextHandles String String
  deriving (Show)

data ConeCmd
  = ConeList
  | ConeCreate String String (Maybe String)
  | ConeGet String
  | ConeDelete String
  | ConeChat String [String]
  | ConeSetHead String String
  deriving (Show)

-- ============================================================================
-- Parsers
-- ============================================================================

commandParser :: Parser Command
commandParser = subparser
  ( command "health" (info (pure Health) (progDesc "Check plexus health status"))
 <> command "bash" (info (Bash <$> bashParser <**> helper) (progDesc "Bash shell operations"))
 <> command "arbor" (info (Arbor <$> arborParser <**> helper) (progDesc "Arbor tree storage operations"))
 <> command "cone" (info (Cone <$> coneParser <**> helper) (progDesc "Cone LLM operations"))
  )

-- Bash
bashParser :: Parser BashCmd
bashParser = subparser
  ( command "execute" (info executeParser (progDesc "Execute a shell command"))
  )
  where
    executeParser = BashExecute <$> many (argument str (metavar "CMD..."))

-- Arbor
arborParser :: Parser ArborCmd
arborParser = subparser
  ( command "tree" (info (ArborTree <$> arborTreeParser <**> helper) (progDesc "Tree operations"))
 <> command "node" (info (ArborNode <$> arborNodeParser <**> helper) (progDesc "Node operations"))
 <> command "context" (info (ArborContext <$> arborContextParser <**> helper) (progDesc "Context operations"))
  )

arborTreeParser :: Parser ArborTreeCmd
arborTreeParser = subparser
  ( command "list" (info (pure TreeList) (progDesc "List all active trees"))
 <> command "list-scheduled" (info (pure TreeListScheduled) (progDesc "List trees scheduled for deletion"))
 <> command "list-archived" (info (pure TreeListArchived) (progDesc "List archived trees"))
 <> command "create" (info createParser (progDesc "Create a new tree"))
 <> command "get" (info getParser (progDesc "Get full tree data"))
 <> command "get-skeleton" (info getSkeletonParser (progDesc "Get tree structure without node data"))
 <> command "render" (info renderParser (progDesc "Render tree as text visualization"))
 <> command "update-metadata" (info updateMetadataParser (progDesc "Update tree metadata"))
 <> command "claim" (info claimParser (progDesc "Claim tree ownership"))
 <> command "release" (info releaseParser (progDesc "Release tree ownership"))
  )
  where
    createParser = TreeCreate <$> argument str (metavar "OWNER")
    getParser = TreeGet <$> argument str (metavar "TREE_ID")
    getSkeletonParser = TreeGetSkeleton <$> argument str (metavar "TREE_ID")
    renderParser = TreeRender <$> argument str (metavar "TREE_ID")
    updateMetadataParser = TreeUpdateMetadata
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "DESCRIPTION")
    claimParser = TreeClaim
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "OWNER")
      <*> option auto (long "count" <> short 'n' <> value 1 <> metavar "N" <> help "Claim count (default: 1)")
    releaseParser = TreeRelease
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "OWNER")
      <*> option auto (long "count" <> short 'n' <> value 1 <> metavar "N" <> help "Release count (default: 1)")

arborNodeParser :: Parser ArborNodeCmd
arborNodeParser = subparser
  ( command "create-text" (info createTextParser (progDesc "Create a root text node"))
 <> command "create-text-child" (info createTextChildParser (progDesc "Create a child text node"))
 <> command "get" (info getParser (progDesc "Get node data"))
 <> command "children" (info childrenParser (progDesc "Get node children"))
 <> command "parent" (info parentParser (progDesc "Get node parent"))
 <> command "path" (info pathParser (progDesc "Get path from root to node"))
  )
  where
    createTextParser = NodeCreateText
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "CONTENT")
    createTextChildParser = NodeCreateTextChild
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "PARENT_ID")
      <*> argument str (metavar "CONTENT")
    getParser = NodeGet
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")
    childrenParser = NodeChildren
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")
    parentParser = NodeParent
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")
    pathParser = NodePath
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")

arborContextParser :: Parser ArborContextCmd
arborContextParser = subparser
  ( command "leaves" (info leavesParser (progDesc "List leaf nodes"))
 <> command "path" (info pathParser (progDesc "Get full path data to node"))
 <> command "handles" (info handlesParser (progDesc "Get external handles in path"))
  )
  where
    leavesParser = ContextLeaves <$> argument str (metavar "TREE_ID")
    pathParser = ContextPath
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")
    handlesParser = ContextHandles
      <$> argument str (metavar "TREE_ID")
      <*> argument str (metavar "NODE_ID")

-- Cone
coneParser :: Parser ConeCmd
coneParser = subparser
  ( command "list" (info (pure ConeList) (progDesc "List all cones"))
 <> command "create" (info createParser (progDesc "Create a new cone"))
 <> command "get" (info getParser (progDesc "Get cone details"))
 <> command "delete" (info deleteParser (progDesc "Delete a cone"))
 <> command "chat" (info chatParser (progDesc "Chat with a cone (streams response)"))
 <> command "set-head" (info setHeadParser (progDesc "Move cone head to a node"))
  )
  where
    createParser = ConeCreate
      <$> argument str (metavar "NAME")
      <*> argument str (metavar "MODEL")
      <*> optional (argument str (metavar "SYSTEM_PROMPT"))
    getParser = ConeGet <$> argument str (metavar "CONE_ID")
    deleteParser = ConeDelete <$> argument str (metavar "CONE_ID")
    chatParser = ConeChat
      <$> argument str (metavar "CONE_ID")
      <*> many (argument str (metavar "PROMPT..."))
    setHeadParser = ConeSetHead
      <$> argument str (metavar "CONE_ID")
      <*> argument str (metavar "NODE_ID")

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  cmd <- customExecParser prefs opts
  runCommand cmd
  where
    prefs = defaultPrefs { prefShowHelpOnEmpty = True }
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "CLI for Plexus - LLM orchestration substrate"
     <> header "symbols-cli - Haskell client for Plexus RPC"
      )

-- ============================================================================
-- Command Runners
-- ============================================================================

runCommand :: Command -> IO ()
runCommand Health = runHealth
runCommand (Bash cmd) = runBashCmd cmd
runCommand (Arbor cmd) = runArborCmd cmd
runCommand (Cone cmd) = runConeCmd cmd

-- Health
runHealth :: IO ()
runHealth = do
  conn <- connect defaultConfig
  S.mapM_ printHealthEvent $ Health.check conn
  disconnect conn

printHealthEvent :: Health.HealthEvent -> IO ()
printHealthEvent (Health.Status s uptime ts) =
  putStrLn $ "Status: " <> T.unpack s <> " (uptime: " <> show uptime <> "s)"

-- Bash
runBashCmd :: BashCmd -> IO ()
runBashCmd (BashExecute args) = do
  let cmd = T.unwords $ map T.pack args
  conn <- connect defaultConfig
  exitCode <- S.foldM_ handleBashEvent (pure ExitSuccess) pure $ Bash.execute conn cmd
  disconnect conn
  exitWith exitCode

handleBashEvent :: ExitCode -> Bash.BashEvent -> IO ExitCode
handleBashEvent _ (Bash.Stdout line) = T.putStrLn line >> pure ExitSuccess
handleBashEvent _ (Bash.Stderr line) = T.hPutStrLn stderr line >> pure ExitSuccess
handleBashEvent _ (Bash.Exit code)
  | code == 0 = pure ExitSuccess
  | otherwise = pure (ExitFailure code)

-- Arbor
runArborCmd :: ArborCmd -> IO ()
runArborCmd (ArborTree cmd) = runArborTreeCmd cmd
runArborCmd (ArborNode cmd) = runArborNodeCmd cmd
runArborCmd (ArborContext cmd) = runArborContextCmd cmd

runArborTreeCmd :: ArborTreeCmd -> IO ()
runArborTreeCmd TreeList = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeList conn
runArborTreeCmd TreeListScheduled = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeListScheduled conn
runArborTreeCmd TreeListArchived = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeListArchived conn
runArborTreeCmd (TreeCreate owner) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeCreate conn Nothing (T.pack owner)
runArborTreeCmd (TreeGet treeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeGet conn (T.pack treeId)
runArborTreeCmd (TreeGetSkeleton treeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeGetSkeleton conn (T.pack treeId)
runArborTreeCmd (TreeRender treeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeRender conn (T.pack treeId)
runArborTreeCmd (TreeUpdateMetadata treeId desc) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeUpdateMetadata conn (T.pack treeId) (object ["description" .= desc])
runArborTreeCmd (TreeClaim treeId owner count) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeClaim conn (T.pack treeId) (T.pack owner) count
runArborTreeCmd (TreeRelease treeId owner count) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeRelease conn (T.pack treeId) (T.pack owner) count

runArborNodeCmd :: ArborNodeCmd -> IO ()
runArborNodeCmd (NodeCreateText treeId content) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeCreateText conn (T.pack treeId) Nothing (T.pack content) Nothing
runArborNodeCmd (NodeCreateTextChild treeId parentId content) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeCreateText conn (T.pack treeId) (Just $ T.pack parentId) (T.pack content) Nothing
runArborNodeCmd (NodeGet treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGet conn (T.pack treeId) (T.pack nodeId)
runArborNodeCmd (NodeChildren treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetChildren conn (T.pack treeId) (T.pack nodeId)
runArborNodeCmd (NodeParent treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetParent conn (T.pack treeId) (T.pack nodeId)
runArborNodeCmd (NodePath treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetPath conn (T.pack treeId) (T.pack nodeId)

runArborContextCmd :: ArborContextCmd -> IO ()
runArborContextCmd (ContextLeaves treeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextListLeaves conn (T.pack treeId)
runArborContextCmd (ContextPath treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextGetPath conn (T.pack treeId) (T.pack nodeId)
runArborContextCmd (ContextHandles treeId nodeId) = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextGetHandles conn (T.pack treeId) (T.pack nodeId)

-- Cone
runConeCmd :: ConeCmd -> IO ()
runConeCmd ConeList = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneList conn
runConeCmd (ConeCreate name model systemPrompt) = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneCreate conn (T.pack name) (T.pack model) (T.pack <$> systemPrompt) Nothing
runConeCmd (ConeGet coneId) = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneGet conn (T.pack coneId)
runConeCmd (ConeDelete coneId) = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneDelete conn (T.pack coneId)
runConeCmd (ConeChat coneId promptWords) = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneChat conn (T.pack coneId) (T.unwords $ map T.pack promptWords)
runConeCmd (ConeSetHead coneId nodeId) = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneSetHead conn (T.pack coneId) (T.pack nodeId)

-- ============================================================================
-- Event Printers
-- ============================================================================

printConeEvent :: Cone.ConeEvent -> IO ()
printConeEvent (Cone.ConeCreated coneId pos) =
  putStrLn $ "Cone created: " <> T.unpack coneId <> " at " <> T.unpack (Cone.positionTreeId pos) <> ":" <> T.unpack (Cone.positionNodeId pos)
printConeEvent (Cone.ConeDeleted coneId) =
  putStrLn $ "Cone deleted: " <> T.unpack coneId
printConeEvent (Cone.ConeUpdated coneId) =
  putStrLn $ "Cone updated: " <> T.unpack coneId
printConeEvent (Cone.ConeData cone) =
  putStrLn $ "Cone: " <> T.unpack (Cone.coneConfigName cone) <> " (" <> T.unpack (Cone.coneConfigId cone) <> ") model=" <> T.unpack (Cone.coneConfigModelId cone)
printConeEvent (Cone.ConeList cones) = do
  putStrLn $ "Cones: " <> show (length cones)
  mapM_ (\c -> putStrLn $ "  " <> T.unpack (Cone.coneInfoName c) <> " (" <> T.unpack (Cone.coneInfoId c) <> ")") cones
printConeEvent (Cone.ChatStart coneId pos) =
  putStrLn $ "Chat started: " <> T.unpack coneId
printConeEvent (Cone.ChatContent _ content) =
  T.putStr content
printConeEvent (Cone.ChatComplete coneId newHead usage) = do
  putStrLn ""
  putStrLn $ "Chat complete: head=" <> T.unpack (Cone.positionNodeId newHead)
printConeEvent (Cone.HeadUpdated coneId oldHead newHead) =
  putStrLn $ "Head updated: " <> T.unpack (Cone.positionNodeId oldHead) <> " -> " <> T.unpack (Cone.positionNodeId newHead)
printConeEvent (Cone.ConeError msg) =
  T.hPutStrLn stderr $ "Error: " <> msg

-- Helpers
withConn :: (Arbor.PlexusConnection -> IO ()) -> IO ()
withConn action = do
  conn <- connect defaultConfig
  action conn
  disconnect conn

printArborEvent :: Arbor.ArborEvent -> IO ()
printArborEvent (Arbor.TreeCreated treeId) =
  putStrLn $ "Tree created: " <> T.unpack treeId
printArborEvent (Arbor.TreeDeleted treeId) =
  putStrLn $ "Tree deleted: " <> T.unpack treeId
printArborEvent (Arbor.TreeUpdated treeId) =
  putStrLn $ "Tree updated: " <> T.unpack treeId
printArborEvent (Arbor.TreeList treeIds) =
  putStrLn $ "Trees: " <> show (map T.unpack treeIds)
printArborEvent (Arbor.TreeClaimed treeId ownerId newCount) =
  putStrLn $ "Tree claimed: " <> T.unpack treeId <> " by " <> T.unpack ownerId <> " (count: " <> show newCount <> ")"
printArborEvent (Arbor.TreeReleased treeId ownerId newCount) =
  putStrLn $ "Tree released: " <> T.unpack treeId <> " by " <> T.unpack ownerId <> " (count: " <> show newCount <> ")"
printArborEvent (Arbor.TreeScheduledDeletion treeId scheduledAt) =
  putStrLn $ "Tree scheduled for deletion: " <> T.unpack treeId <> " at " <> show scheduledAt
printArborEvent (Arbor.TreeArchived treeId archivedAt) =
  putStrLn $ "Tree archived: " <> T.unpack treeId <> " at " <> show archivedAt
printArborEvent (Arbor.TreeRefs treeId refs) =
  putStrLn $ "Tree refs: " <> T.unpack treeId <> " - " <> show refs
printArborEvent (Arbor.TreeData tree) =
  putStrLn $ "Tree data: " <> show tree
printArborEvent (Arbor.TreeSkeletonData skeleton) =
  putStrLn $ "Tree skeleton: " <> show skeleton
printArborEvent (Arbor.NodeCreated treeId nodeId parent) =
  putStrLn $ "Node created: " <> T.unpack nodeId <> " in tree " <> T.unpack treeId <> maybe "" (\p -> " (parent: " <> T.unpack p <> ")") parent
printArborEvent (Arbor.NodeData treeId node) =
  putStrLn $ "Node data: " <> show node
printArborEvent (Arbor.NodeChildren treeId nodeId children) =
  putStrLn $ "Node children: " <> show (map T.unpack children)
printArborEvent (Arbor.NodeParent treeId nodeId parent) =
  putStrLn $ "Node parent: " <> maybe "none" T.unpack parent
printArborEvent (Arbor.ContextPath treeId path) =
  putStrLn $ "Context path: " <> show (map T.unpack path)
printArborEvent (Arbor.ContextPathData treeId nodes) =
  putStrLn $ "Context path data: " <> show nodes
printArborEvent (Arbor.ContextHandles treeId handles) =
  putStrLn $ "Context handles: " <> show handles
printArborEvent (Arbor.ContextLeaves treeId leaves) =
  putStrLn $ "Context leaves: " <> show (map T.unpack leaves)
printArborEvent (Arbor.TreesScheduled treeIds) =
  putStrLn $ "Scheduled trees: " <> show (map T.unpack treeIds)
printArborEvent (Arbor.TreesArchived treeIds) =
  putStrLn $ "Archived trees: " <> show (map T.unpack treeIds)
printArborEvent (Arbor.TreeRenderResult _ render) =
  T.putStrLn render
