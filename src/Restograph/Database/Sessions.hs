module Restograph.Database.Sessions where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import qualified Restograph.Database.Statements as St
import Data.Vector (Vector)
import qualified Data.Vector as V(map, toList)
import Restograph.Model (Node(..), NodeId(..))

createNode :: Text -> Session (Maybe Int32)
createNode label = do
  statement label St.createNode

deleteNode :: Int32 -> Session Bool
deleteNode nodeId = do
  ra <- statement nodeId St.deleteNode
  return $ ra > 0

renameNode :: Int32 -> Text -> Session Bool
renameNode nodeId label = do
  ra <- statement (nodeId, label) St.renameNode
  return $ ra > 0

linkNodes :: Int32 -> Int32 -> Session Bool
linkNodes left right = do
  ra <- statement (left, right) St.linkNodes
  return $ ra > 0

listNodes :: Session [Node]
listNodes = do
  rows <- statement () St.listNodes
  return $ V.toList $ V.map (\(id, label) -> Node (NodeId id) label) rows
  
listNodeNeighbors :: Int32 -> Session (Vector Node)
listNodeNeighbors nodeId = do
  rows <- statement nodeId St.listNodeNeighbors
  return $ V.map (\(id, label) -> Node (NodeId id) label) rows
