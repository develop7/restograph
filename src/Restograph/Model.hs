{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Restograph.Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson.Stock (PrefixedSnake, CustomJSON(..))
import GHC.Generics (Generic)
import Data.Int (Int32)

newtype NodeId = NodeId Int32 deriving (Generic, ToJSON, FromJSON)

data Node = Node {nodeId :: NodeId, nodeLabel :: Text}
  deriving (Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "node" Node

data NodeReq = NodeReq {nrLabel :: Text}
  deriving (Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "nr" NodeReq
