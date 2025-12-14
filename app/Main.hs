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
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Health
    ("health":_) -> runHealth

    -- Bash
    ("bash":"execute":cmd) -> runBash (T.unwords $ map T.pack cmd)

    -- Arbor Tree Operations
    ("arbor":"tree":"list":_) -> runArborTreeList
    ("arbor":"tree":"list-scheduled":_) -> runArborTreeListScheduled
    ("arbor":"tree":"list-archived":_) -> runArborTreeListArchived
    ("arbor":"tree":"create":owner) -> runArborTreeCreate (T.unwords $ map T.pack owner)
    ("arbor":"tree":"get":treeId:_) -> runArborTreeGet (T.pack treeId)
    ("arbor":"tree":"get-skeleton":treeId:_) -> runArborTreeGetSkeleton (T.pack treeId)
    ("arbor":"tree":"render":treeId:_) -> runArborTreeRender (T.pack treeId)
    ("arbor":"tree":"update-metadata":treeId:rest) -> runArborTreeUpdateMetadata (T.pack treeId) (T.unwords $ map T.pack rest)
    ("arbor":"tree":"claim":treeId:ownerId:rest) -> runArborTreeClaim (T.pack treeId) (T.pack ownerId) (parseCount rest)
    ("arbor":"tree":"release":treeId:ownerId:rest) -> runArborTreeRelease (T.pack treeId) (T.pack ownerId) (parseCount rest)

    -- Arbor Node Operations
    ("arbor":"node":"create-text":treeId:content) -> runArborNodeCreateText (T.pack treeId) Nothing (T.unwords $ map T.pack content)
    ("arbor":"node":"create-text-child":treeId:parentId:content) -> runArborNodeCreateText (T.pack treeId) (Just $ T.pack parentId) (T.unwords $ map T.pack content)
    ("arbor":"node":"get":treeId:nodeId:_) -> runArborNodeGet (T.pack treeId) (T.pack nodeId)
    ("arbor":"node":"children":treeId:nodeId:_) -> runArborNodeGetChildren (T.pack treeId) (T.pack nodeId)
    ("arbor":"node":"parent":treeId:nodeId:_) -> runArborNodeGetParent (T.pack treeId) (T.pack nodeId)
    ("arbor":"node":"path":treeId:nodeId:_) -> runArborNodeGetPath (T.pack treeId) (T.pack nodeId)

    -- Arbor Context Operations
    ("arbor":"context":"leaves":treeId:_) -> runArborContextListLeaves (T.pack treeId)
    ("arbor":"context":"path":treeId:nodeId:_) -> runArborContextGetPath (T.pack treeId) (T.pack nodeId)
    ("arbor":"context":"handles":treeId:nodeId:_) -> runArborContextGetHandles (T.pack treeId) (T.pack nodeId)

    -- Cone Operations
    ("cone":"list":_) -> runConeList
    ("cone":"create":name:model:rest) -> runConeCreate (T.pack name) (T.pack model) (listToMaybe $ map T.pack rest)
    ("cone":"get":coneId:_) -> runConeGet (T.pack coneId)
    ("cone":"delete":coneId:_) -> runConeDelete (T.pack coneId)
    ("cone":"chat":coneId:prompt) -> runConeChat (T.pack coneId) (T.unwords $ map T.pack prompt)
    ("cone":"set-head":coneId:nodeId:_) -> runConeSetHead (T.pack coneId) (T.pack nodeId)

    -- Legacy aliases (loom -> arbor)
    ("loom":"tree":"list":_) -> runArborTreeList
    ("loom":"tree":"create":owner) -> runArborTreeCreate (T.unwords $ map T.pack owner)
    ("loom":"list":_) -> runArborTreeList
    ("loom":"create":owner) -> runArborTreeCreate (T.unwords $ map T.pack owner)

    -- Subcommand help
    ["cone"] -> printConeHelp
    ["arbor"] -> printArborHelp
    ("arbor":"tree":_) -> printArborTreeHelp
    ("arbor":"node":_) -> printArborNodeHelp
    ("arbor":"context":_) -> printArborContextHelp
    ["bash"] -> printBashHelp

    _ -> printUsage
  where
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x
    parseCount [] = 1
    parseCount (x:_) = read x

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
runBash :: Text -> IO ()
runBash cmd = do
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

-- Arbor Tree Operations
runArborTreeList :: IO ()
runArborTreeList = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeList conn

runArborTreeListScheduled :: IO ()
runArborTreeListScheduled = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeListScheduled conn

runArborTreeListArchived :: IO ()
runArborTreeListArchived = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeListArchived conn

runArborTreeCreate :: Text -> IO ()
runArborTreeCreate owner = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeCreate conn Nothing owner

runArborTreeGet :: Text -> IO ()
runArborTreeGet treeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeGet conn treeId

runArborTreeGetSkeleton :: Text -> IO ()
runArborTreeGetSkeleton treeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeGetSkeleton conn treeId

runArborTreeRender :: Text -> IO ()
runArborTreeRender treeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeRender conn treeId

runArborTreeUpdateMetadata :: Text -> Text -> IO ()
runArborTreeUpdateMetadata treeId metaStr = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeUpdateMetadata conn treeId (object ["description" .= metaStr])

runArborTreeClaim :: Text -> Text -> Int -> IO ()
runArborTreeClaim treeId ownerId count = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeClaim conn treeId ownerId count

runArborTreeRelease :: Text -> Text -> Int -> IO ()
runArborTreeRelease treeId ownerId count = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.treeRelease conn treeId ownerId count

-- Arbor Node Operations
runArborNodeCreateText :: Text -> Maybe Text -> Text -> IO ()
runArborNodeCreateText treeId parent content = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeCreateText conn treeId parent content Nothing

runArborNodeGet :: Text -> Text -> IO ()
runArborNodeGet treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGet conn treeId nodeId

runArborNodeGetChildren :: Text -> Text -> IO ()
runArborNodeGetChildren treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetChildren conn treeId nodeId

runArborNodeGetParent :: Text -> Text -> IO ()
runArborNodeGetParent treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetParent conn treeId nodeId

runArborNodeGetPath :: Text -> Text -> IO ()
runArborNodeGetPath treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.nodeGetPath conn treeId nodeId

-- Arbor Context Operations
runArborContextListLeaves :: Text -> IO ()
runArborContextListLeaves treeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextListLeaves conn treeId

runArborContextGetPath :: Text -> Text -> IO ()
runArborContextGetPath treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextGetPath conn treeId nodeId

runArborContextGetHandles :: Text -> Text -> IO ()
runArborContextGetHandles treeId nodeId = withConn $ \conn ->
  S.mapM_ printArborEvent $ Arbor.contextGetHandles conn treeId nodeId

-- Cone Operations
runConeList :: IO ()
runConeList = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneList conn

runConeCreate :: Text -> Text -> Maybe Text -> IO ()
runConeCreate name modelId systemPrompt = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneCreate conn name modelId systemPrompt Nothing

runConeGet :: Text -> IO ()
runConeGet coneId = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneGet conn coneId

runConeDelete :: Text -> IO ()
runConeDelete coneId = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneDelete conn coneId

runConeChat :: Text -> Text -> IO ()
runConeChat coneId prompt = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneChat conn coneId prompt

runConeSetHead :: Text -> Text -> IO ()
runConeSetHead coneId nodeId = withConn $ \conn ->
  S.mapM_ printConeEvent $ Cone.coneSetHead conn coneId nodeId

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
  T.putStr content  -- Stream content without newline
printConeEvent (Cone.ChatComplete coneId newHead usage) = do
  putStrLn ""  -- Newline after streaming content
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

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: symbols-cli <command> [args...]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  health                              Check plexus health status"
  putStrLn "  bash execute <cmd>                  Execute a bash command"
  putStrLn ""
  putStrLn "Arbor Tree Operations:"
  putStrLn "  arbor tree list                     List all active trees"
  putStrLn "  arbor tree list-scheduled           List trees scheduled for deletion"
  putStrLn "  arbor tree list-archived            List archived trees"
  putStrLn "  arbor tree create <owner>           Create a new tree"
  putStrLn "  arbor tree get <tree_id>            Get full tree data"
  putStrLn "  arbor tree get-skeleton <tree_id>   Get tree structure without node data"
  putStrLn "  arbor tree render <tree_id>         Render tree as text visualization"
  putStrLn "  arbor tree update-metadata <tree_id> <desc>  Update tree metadata"
  putStrLn "  arbor tree claim <tree_id> <owner> [count]   Claim tree ownership"
  putStrLn "  arbor tree release <tree_id> <owner> [count] Release tree ownership"
  putStrLn ""
  putStrLn "Arbor Node Operations:"
  putStrLn "  arbor node create-text <tree_id> <content>              Create root text node"
  putStrLn "  arbor node create-text-child <tree_id> <parent> <content>  Create child text node"
  putStrLn "  arbor node get <tree_id> <node_id>                      Get node data"
  putStrLn "  arbor node children <tree_id> <node_id>                 Get node children"
  putStrLn "  arbor node parent <tree_id> <node_id>                   Get node parent"
  putStrLn "  arbor node path <tree_id> <node_id>                     Get path from root to node"
  putStrLn ""
  putStrLn "Arbor Context Operations:"
  putStrLn "  arbor context leaves <tree_id>                    List leaf nodes"
  putStrLn "  arbor context path <tree_id> <node_id>            Get full path data to node"
  putStrLn "  arbor context handles <tree_id> <node_id>         Get external handles in path"
  putStrLn ""
  putStrLn "Cone Operations (LLM growth cones):"
  putStrLn "  cone list                                         List all cones"
  putStrLn "  cone create <name> <model> [system_prompt]        Create a new cone"
  putStrLn "  cone get <cone_id>                                Get cone details"
  putStrLn "  cone delete <cone_id>                             Delete a cone"
  putStrLn "  cone chat <cone_id> <prompt>                      Chat with a cone (streams response)"
  putStrLn "  cone set-head <cone_id> <node_id>                 Move cone head to a node"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli health"
  putStrLn "  symbols-cli bash execute echo hello world"
  putStrLn "  symbols-cli arbor tree list"
  putStrLn "  symbols-cli cone create mycone gpt-4o-mini"
  putStrLn "  symbols-cli cone chat <cone_id> Hello, how are you?"

printConeHelp :: IO ()
printConeHelp = do
  putStrLn "Usage: symbols-cli cone <command> [args...]"
  putStrLn ""
  putStrLn "Cone Operations (LLM growth cones):"
  putStrLn "  list                                  List all cones"
  putStrLn "  create <name> <model> [system_prompt] Create a new cone"
  putStrLn "  get <cone_id>                         Get cone details"
  putStrLn "  delete <cone_id>                      Delete a cone"
  putStrLn "  chat <cone_id> <prompt>               Chat with a cone (streams response)"
  putStrLn "  set-head <cone_id> <node_id>          Move cone head to a node"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli cone list"
  putStrLn "  symbols-cli cone create mycone gpt-4o-mini"
  putStrLn "  symbols-cli cone create mycone gpt-4o-mini \"You are a helpful assistant\""
  putStrLn "  symbols-cli cone chat <cone_id> Hello, how are you?"

printArborHelp :: IO ()
printArborHelp = do
  putStrLn "Usage: symbols-cli arbor <subcommand> [args...]"
  putStrLn ""
  putStrLn "Subcommands:"
  putStrLn "  tree     Tree operations (create, list, get, render, etc.)"
  putStrLn "  node     Node operations (create, get, children, parent, etc.)"
  putStrLn "  context  Context operations (leaves, path, handles)"
  putStrLn ""
  putStrLn "Run 'symbols-cli arbor <subcommand>' for more details."

printArborTreeHelp :: IO ()
printArborTreeHelp = do
  putStrLn "Usage: symbols-cli arbor tree <command> [args...]"
  putStrLn ""
  putStrLn "Tree Operations:"
  putStrLn "  list                                  List all active trees"
  putStrLn "  list-scheduled                        List trees scheduled for deletion"
  putStrLn "  list-archived                         List archived trees"
  putStrLn "  create <owner>                        Create a new tree"
  putStrLn "  get <tree_id>                         Get full tree data"
  putStrLn "  get-skeleton <tree_id>                Get tree structure without node data"
  putStrLn "  render <tree_id>                      Render tree as text visualization"
  putStrLn "  update-metadata <tree_id> <desc>      Update tree metadata"
  putStrLn "  claim <tree_id> <owner> [count]       Claim tree ownership"
  putStrLn "  release <tree_id> <owner> [count]     Release tree ownership"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli arbor tree list"
  putStrLn "  symbols-cli arbor tree create alice"
  putStrLn "  symbols-cli arbor tree render <tree_id>"

printArborNodeHelp :: IO ()
printArborNodeHelp = do
  putStrLn "Usage: symbols-cli arbor node <command> [args...]"
  putStrLn ""
  putStrLn "Node Operations:"
  putStrLn "  create-text <tree_id> <content>                    Create root text node"
  putStrLn "  create-text-child <tree_id> <parent> <content>     Create child text node"
  putStrLn "  get <tree_id> <node_id>                            Get node data"
  putStrLn "  children <tree_id> <node_id>                       Get node children"
  putStrLn "  parent <tree_id> <node_id>                         Get node parent"
  putStrLn "  path <tree_id> <node_id>                           Get path from root to node"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli arbor node create-text <tree_id> \"Hello world\""
  putStrLn "  symbols-cli arbor node children <tree_id> <node_id>"

printArborContextHelp :: IO ()
printArborContextHelp = do
  putStrLn "Usage: symbols-cli arbor context <command> [args...]"
  putStrLn ""
  putStrLn "Context Operations:"
  putStrLn "  leaves <tree_id>                      List leaf nodes"
  putStrLn "  path <tree_id> <node_id>              Get full path data to node"
  putStrLn "  handles <tree_id> <node_id>           Get external handles in path"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli arbor context leaves <tree_id>"
  putStrLn "  symbols-cli arbor context path <tree_id> <node_id>"

printBashHelp :: IO ()
printBashHelp = do
  putStrLn "Usage: symbols-cli bash <command> [args...]"
  putStrLn ""
  putStrLn "Bash Operations:"
  putStrLn "  execute <cmd>    Execute a shell command (streams output)"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  symbols-cli bash execute echo hello world"
  putStrLn "  symbols-cli bash execute ls -la"
