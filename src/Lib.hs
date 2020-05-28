{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( someFunc,
    waiApp,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Database.PostgreSQL.LibPQ (Connection)
import GHC.Generics
import Hasql.Connection ()
import Network.Wai (Application)
import Servant.API ((:<|>) (..), (:<|>), (:>), Accept, Capture, Delete, DeleteNoContent, Get, JSON, NoContent (..), Put, PutNoContent, ReqBody)
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant.Server (Server, serve)
import Servant.Server.Generic (AsServer, genericServe, genericServer)

data Node = Node {nodeId :: Int, nodeLabel :: Text} deriving (Generic)

instance ToJSON Node

data NodeReq = NodeReq {nrLabel :: Text} deriving (Generic)

instance FromJSON NodeReq

type API = "graph" :> GraphAPI

type GraphAPI = "node" :> ToServantApi NodeAPI :<|> "link" :> ToServantApi LinkAPI

data NodeAPI r
  = NodeAPI
      { create :: r :- ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        delete :: r :- Capture "id" Int :> DeleteNoContent '[JSON] NoContent,
        rename :: r :- Capture "id " Int :> "label" :> ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        neighbours :: r :- Capture "id " Int :> "neighbours" :> Get '[JSON] [Node],
        list :: r :- Get '[JSON] [Node]
      }
  deriving (Generic)

data LinkAPI r
  = LinkAPI
      { new :: r :- Capture "id_from" Int :> Capture "id_to" Int :> PutNoContent '[JSON] NoContent
      }
  deriving (Generic)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

server :: Connection -> Server API
server conn = genericServer nodeAPI :<|> genericServer linkAPI
  where
    nodeAPI :: NodeAPI AsServer
    nodeAPI =
      NodeAPI
        { list = return [Node {nodeId = 10, nodeLabel = "wat"}],
          create = \NodeReq {nrLabel = nl} -> return $ Node {nodeId = 100, nodeLabel = nl},
          delete = \_id -> return NoContent,
          rename = \_id NodeReq {nrLabel = nl} -> return $ Node {nodeId = 42, nodeLabel = nl},
          neighbours = \_id -> return [Node 14 "Hurr", Node 15 "Durr"]
        }
    linkAPI :: LinkAPI AsServer
    linkAPI = LinkAPI {new = (\fro to -> return NoContent)}

api :: Proxy API
api = Proxy

waiApp :: Connection -> Application
waiApp conn = do
  serve api $ server conn
