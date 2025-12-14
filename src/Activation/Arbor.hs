-- | Typed API for the Arbor activation - conversation tree storage
--
-- The arbor is the dendritic branching structure that stores conversation topology.
-- It provides passive storage - it doesn't decide what to add, it holds what has been added.
module Activation.Arbor
  ( -- * Re-exports
    PlexusConnection
    -- * Types
  , ArborEvent(..)
  , TreeId
  , NodeId
  , Tree(..)
  , Node(..)
  , NodeType(..)
  , Handle(..)
  , ResourceState(..)
  , ResourceRefs(..)
  , TreeSkeleton(..)
  , NodeSkeleton(..)

    -- * Tree Operations
  , treeCreate
  , treeGet
  , treeGetSkeleton
  , treeList
  , treeUpdateMetadata
  , treeClaim
  , treeRelease
  , treeListScheduled
  , treeListArchived

    -- * Node Operations
  , nodeCreateText
  , nodeCreateExternal
  , nodeGet
  , nodeGetChildren
  , nodeGetParent
  , nodeGetPath

    -- * Context Operations
  , contextListLeaves
  , contextGetPath
  , contextGetHandles

    -- * Rendering
  , treeRender
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import Plexus.Client (PlexusConnection, plexusRpc)
import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- Core Types
-- ============================================================================

-- | UUID string identifier for trees
type TreeId = Text

-- | UUID string identifier for nodes
type NodeId = Text

-- | Handle pointing to external data with versioning
data Handle = Handle
  { handleSource        :: Text
  , handleSourceVersion :: Text
  , handleIdentifier    :: Text
  , handleMetadata      :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Node type discriminator
data NodeType
  = TextNode { nodeContent :: Text }
  | ExternalNode { nodeHandle :: Handle }
  deriving stock (Show, Eq, Generic)

instance FromJSON NodeType where
  parseJSON = withObject "NodeType" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "text"     -> TextNode <$> o .: "content"
      "external" -> ExternalNode <$> o .: "handle"
      _          -> fail $ "Unknown node type: " <> T.unpack typ

instance ToJSON NodeType where
  toJSON (TextNode content) = object
    [ "type" .= ("text" :: Text)
    , "content" .= content
    ]
  toJSON (ExternalNode handle) = object
    [ "type" .= ("external" :: Text)
    , "handle" .= handle
    ]

-- | Resource state in deletion lifecycle
data ResourceState
  = Active
  | ScheduledDelete
  | Archived
  deriving stock (Show, Eq, Generic)

instance FromJSON ResourceState where
  parseJSON = withText "ResourceState" $ \case
    "active"           -> pure Active
    "scheduled_delete" -> pure ScheduledDelete
    "archived"         -> pure Archived
    other              -> fail $ "Unknown resource state: " <> T.unpack other

instance ToJSON ResourceState where
  toJSON Active          = String "active"
  toJSON ScheduledDelete = String "scheduled_delete"
  toJSON Archived        = String "archived"

-- | Reference counting information
data ResourceRefs = ResourceRefs
  { refCount :: Int
  , owners   :: Map Text Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A node in the conversation tree
data Node = Node
  { nodeId                 :: NodeId
  , nodeParent             :: Maybe NodeId
  , nodeChildren           :: [NodeId]
  , nodeData               :: NodeType
  , nodeState              :: Maybe ResourceState
  , nodeRefs               :: Maybe ResourceRefs
  , nodeScheduledDeletionAt :: Maybe Int
  , nodeArchivedAt         :: Maybe Int
  , nodeCreatedAt          :: Int
  , nodeMetadata           :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o ->
    Node
      <$> o .: "id"
      <*> o .:? "parent"
      <*> o .: "children"
      <*> o .: "data"
      <*> o .:? "state"
      <*> o .:? "refs"
      <*> o .:? "scheduled_deletion_at"
      <*> o .:? "archived_at"
      <*> o .: "created_at"
      <*> o .:? "metadata"

-- | A conversation tree
data Tree = Tree
  { treeId                 :: TreeId
  , treeRoot               :: NodeId
  , treeNodes              :: Map NodeId Node
  , treeState              :: Maybe ResourceState
  , treeRefs               :: Maybe ResourceRefs
  , treeScheduledDeletionAt :: Maybe Int
  , treeArchivedAt         :: Maybe Int
  , treeCreatedAt          :: Int
  , treeUpdatedAt          :: Int
  , treeMetadata           :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Tree where
  parseJSON = withObject "Tree" $ \o ->
    Tree
      <$> o .: "id"
      <*> o .: "root"
      <*> o .: "nodes"
      <*> o .:? "state"
      <*> o .:? "refs"
      <*> o .:? "scheduled_deletion_at"
      <*> o .:? "archived_at"
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "metadata"

-- | Lightweight node representation
data NodeSkeleton = NodeSkeleton
  { nodeSkeletonId       :: NodeId
  , nodeSkeletonParent   :: Maybe NodeId
  , nodeSkeletonChildren :: [NodeId]
  , nodeSkeletonType     :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NodeSkeleton where
  parseJSON = withObject "NodeSkeleton" $ \o ->
    NodeSkeleton
      <$> o .: "id"
      <*> o .:? "parent"
      <*> o .: "children"
      <*> o .: "node_type"

-- | Lightweight tree structure
data TreeSkeleton = TreeSkeleton
  { treeSkeletonId    :: TreeId
  , treeSkeletonRoot  :: NodeId
  , treeSkeletonNodes :: Map NodeId NodeSkeleton
  , treeSkeletonState :: Maybe ResourceState
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TreeSkeleton where
  parseJSON = withObject "TreeSkeleton" $ \o ->
    TreeSkeleton
      <$> o .: "id"
      <*> o .: "root"
      <*> o .: "nodes"
      <*> o .:? "state"

-- ============================================================================
-- Stream Events
-- ============================================================================

-- | Events emitted by Arbor operations
data ArborEvent
  = TreeCreated { eventTreeId :: TreeId }
  | TreeDeleted { eventTreeId :: TreeId }
  | TreeUpdated { eventTreeId :: TreeId }
  | TreeList { eventTreeIds :: [TreeId] }
  | TreeClaimed { eventTreeId :: TreeId, eventOwnerId :: Text, eventNewCount :: Int }
  | TreeReleased { eventTreeId :: TreeId, eventOwnerId :: Text, eventNewCount :: Int }
  | TreeScheduledDeletion { eventTreeId :: TreeId, eventScheduledAt :: Int }
  | TreeArchived { eventTreeId :: TreeId, eventArchivedAt :: Int }
  | TreeRefs { eventTreeId :: TreeId, eventRefs :: ResourceRefs }
  | TreeData { eventTree :: Tree }
  | TreeSkeletonData { eventSkeleton :: TreeSkeleton }
  | NodeCreated { eventTreeId :: TreeId, eventNodeId :: NodeId, eventParent :: Maybe NodeId }
  | NodeData { eventTreeId :: TreeId, eventNode :: Node }
  | NodeChildren { eventTreeId :: TreeId, eventNodeId :: NodeId, eventChildren :: [NodeId] }
  | NodeParent { eventTreeId :: TreeId, eventNodeId :: NodeId, eventParent :: Maybe NodeId }
  | ContextPath { eventTreeId :: TreeId, eventPath :: [NodeId] }
  | ContextPathData { eventTreeId :: TreeId, eventNodes :: [Node] }
  | ContextHandles { eventTreeId :: TreeId, eventHandles :: [Handle] }
  | ContextLeaves { eventTreeId :: TreeId, eventLeaves :: [NodeId] }
  | TreesScheduled { eventTreeIds :: [TreeId] }
  | TreesArchived { eventTreeIds :: [TreeId] }
  | TreeRenderResult { eventTreeId :: TreeId, eventRender :: Text }
  deriving stock (Show, Eq, Generic)

instance FromJSON ArborEvent where
  parseJSON = withObject "ArborEvent" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "tree_created"           -> TreeCreated <$> o .: "tree_id"
      "tree_deleted"           -> TreeDeleted <$> o .: "tree_id"
      "tree_updated"           -> TreeUpdated <$> o .: "tree_id"
      "tree_list"              -> TreeList <$> o .: "tree_ids"
      "tree_claimed"           -> TreeClaimed <$> o .: "tree_id" <*> o .: "owner_id" <*> o .: "new_count"
      "tree_released"          -> TreeReleased <$> o .: "tree_id" <*> o .: "owner_id" <*> o .: "new_count"
      "tree_scheduled_deletion" -> TreeScheduledDeletion <$> o .: "tree_id" <*> o .: "scheduled_at"
      "tree_archived"          -> TreeArchived <$> o .: "tree_id" <*> o .: "archived_at"
      "tree_refs"              -> TreeRefs <$> o .: "tree_id" <*> o .: "refs"
      "tree_data"              -> TreeData <$> o .: "tree"
      "tree_skeleton"          -> TreeSkeletonData <$> o .: "skeleton"
      "node_created"           -> NodeCreated <$> o .: "tree_id" <*> o .: "node_id" <*> o .:? "parent"
      "node_data"              -> NodeData <$> o .: "tree_id" <*> o .: "node"
      "node_children"          -> NodeChildren <$> o .: "tree_id" <*> o .: "node_id" <*> o .: "children"
      "node_parent"            -> NodeParent <$> o .: "tree_id" <*> o .: "node_id" <*> o .:? "parent"
      "context_path"           -> ContextPath <$> o .: "tree_id" <*> o .: "path"
      "context_path_data"      -> ContextPathData <$> o .: "tree_id" <*> o .: "nodes"
      "context_handles"        -> ContextHandles <$> o .: "tree_id" <*> o .: "handles"
      "context_leaves"         -> ContextLeaves <$> o .: "tree_id" <*> o .: "leaves"
      "trees_scheduled"        -> TreesScheduled <$> o .: "tree_ids"
      "trees_archived"         -> TreesArchived <$> o .: "tree_ids"
      "tree_render"            -> TreeRenderResult <$> o .: "tree_id" <*> o .: "render"
      _                        -> fail $ "Unknown arbor event type: " <> T.unpack typ

-- ============================================================================
-- Helper
-- ============================================================================

extractArborEvent :: PlexusStreamItem -> Maybe ArborEvent
extractArborEvent (StreamData _ contentType dat)
  | contentType == "arbor.event" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractArborEvent _ = Nothing

-- ============================================================================
-- Tree Operations
-- ============================================================================

-- | Create a new conversation tree
treeCreate :: PlexusConnection -> Maybe Value -> Text -> Stream (Of ArborEvent) IO ()
treeCreate conn metadata ownerId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_create" (toJSON [toJSON metadata, toJSON ownerId])

-- | Get a complete tree with all nodes
treeGet :: PlexusConnection -> TreeId -> Stream (Of ArborEvent) IO ()
treeGet conn treeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_get" (toJSON [treeId])

-- | Get lightweight tree structure (nodes without data)
treeGetSkeleton :: PlexusConnection -> TreeId -> Stream (Of ArborEvent) IO ()
treeGetSkeleton conn treeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_get_skeleton" (toJSON [treeId])

-- | List all active trees
treeList :: PlexusConnection -> Stream (Of ArborEvent) IO ()
treeList conn =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_list" (toJSON ([] :: [Value]))

-- | Update tree metadata
treeUpdateMetadata :: PlexusConnection -> TreeId -> Value -> Stream (Of ArborEvent) IO ()
treeUpdateMetadata conn treeId metadata =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_update_metadata" (toJSON [toJSON treeId, metadata])

-- | Claim ownership of a tree (increment reference count)
treeClaim :: PlexusConnection -> TreeId -> Text -> Int -> Stream (Of ArborEvent) IO ()
treeClaim conn treeId ownerId count =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_claim" (toJSON [toJSON treeId, toJSON ownerId, toJSON count])

-- | Release ownership of a tree (decrement reference count)
treeRelease :: PlexusConnection -> TreeId -> Text -> Int -> Stream (Of ArborEvent) IO ()
treeRelease conn treeId ownerId count =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_release" (toJSON [toJSON treeId, toJSON ownerId, toJSON count])

-- | List trees scheduled for deletion
treeListScheduled :: PlexusConnection -> Stream (Of ArborEvent) IO ()
treeListScheduled conn =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_list_scheduled" (toJSON ([] :: [Value]))

-- | List archived trees
treeListArchived :: PlexusConnection -> Stream (Of ArborEvent) IO ()
treeListArchived conn =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_list_archived" (toJSON ([] :: [Value]))

-- ============================================================================
-- Node Operations
-- ============================================================================

-- | Create a text node in a tree
nodeCreateText :: PlexusConnection -> TreeId -> Maybe NodeId -> Text -> Maybe Value -> Stream (Of ArborEvent) IO ()
nodeCreateText conn treeId parent content metadata =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_create_text" (toJSON [toJSON treeId, toJSON parent, toJSON content, toJSON metadata])

-- | Create an external node in a tree
nodeCreateExternal :: PlexusConnection -> TreeId -> Maybe NodeId -> Handle -> Maybe Value -> Stream (Of ArborEvent) IO ()
nodeCreateExternal conn treeId parent handle metadata =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_create_external" (toJSON [toJSON treeId, toJSON parent, toJSON handle, toJSON metadata])

-- | Get a node by ID
nodeGet :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
nodeGet conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_get" (toJSON [treeId, nodeId])

-- | Get the children of a node
nodeGetChildren :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
nodeGetChildren conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_get_children" (toJSON [treeId, nodeId])

-- | Get the parent of a node
nodeGetParent :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
nodeGetParent conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_get_parent" (toJSON [treeId, nodeId])

-- | Get the path from root to a node
nodeGetPath :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
nodeGetPath conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_node_get_path" (toJSON [treeId, nodeId])

-- ============================================================================
-- Context Operations
-- ============================================================================

-- | List all leaf nodes in a tree
contextListLeaves :: PlexusConnection -> TreeId -> Stream (Of ArborEvent) IO ()
contextListLeaves conn treeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_context_list_leaves" (toJSON [treeId])

-- | Get the full path data from root to a node (the dendrite)
contextGetPath :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
contextGetPath conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_context_get_path" (toJSON [treeId, nodeId])

-- | Get all external handles in the path to a node
contextGetHandles :: PlexusConnection -> TreeId -> NodeId -> Stream (Of ArborEvent) IO ()
contextGetHandles conn treeId nodeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_context_get_handles" (toJSON [treeId, nodeId])

-- ============================================================================
-- Rendering
-- ============================================================================

-- | Render tree as text visualization
treeRender :: PlexusConnection -> TreeId -> Stream (Of ArborEvent) IO ()
treeRender conn treeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_render" (toJSON [treeId])
