{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators, GeneralisedNewtypeDeriving #-}

module Lib
  ( waiApp,
  )
where

import Control.Lens ((&), (.~), (?~), mapped)
import Data.Aeson (toJSON)
import Data.Proxy (Proxy (..))
import Data.Swagger (Swagger (..), ToSchema (..), defaultSchemaOptions, description, example, genericDeclareNamedSchema, info, schema, title, version, ToParamSchema)
import Data.Text (Text)
import Database.PostgreSQL.LibPQ (Connection)
import Deriving.Aeson.Stock
import Hasql.Connection ()
import Network.Wai (Application)
import Servant.API ((:<|>) (..), (:<|>), (:>), Capture, DeleteNoContent, Get, JSON, NoContent (..), PlainText, Put, PutNoContent, ReqBody, FromHttpApiData)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Server (Server, serve)
import Servant.Server.Generic (AsServer, genericServer)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

newtype NodeId = NodeId Int deriving (Generic, ToJSON, FromJSON, FromHttpApiData)

instance ToParamSchema NodeId
instance ToSchema NodeId

data Node = Node {nodeId :: NodeId, nodeLabel :: Text}
  deriving (Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "node" Node

instance ToSchema Node where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Graph node"
      & mapped . schema . example ?~ toJSON (Node (NodeId 42) "answer")

data NodeReq = NodeReq {nrLabel :: Text}
  deriving (Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "nr" NodeReq

instance ToSchema NodeReq where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Node-related request data"
      & mapped . schema . example ?~ toJSON (NodeReq "answer")

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> SwaggerAPI :<|> GraphAPI

type GraphAPI = "graph" :> ("node" :> ToServantApi NodeAPI :<|> "link" :> ToServantApi LinkAPI)

data NodeAPI r
  = NodeAPI
      { create :: r :- ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        delete :: r :- Capture "id" NodeId :> DeleteNoContent '[PlainText] NoContent,
        rename :: r :- Capture "id " NodeId :> "label" :> ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        neighbours :: r :- Capture "id " NodeId :> "neighbours" :> Get '[JSON] [Node],
        list :: r :- Get '[JSON] [Node]
      }
  deriving (Generic)

data LinkAPI r
  = LinkAPI
      { new :: r :- Capture "id_from" NodeId :> Capture "id_to" NodeId :> PutNoContent '[JSON] NoContent
      }
  deriving (Generic)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

graphSwagger :: Swagger
graphSwagger =
  toSwagger (Proxy :: Proxy GraphAPI)
    & info . title .~ "Graph manipulation API"
    & info . version .~ "0.1"
    & info . description ?~ "An undirected graph manipulation API"

server :: Connection -> Server API
server _conn =
  swaggerSchemaUIServer graphSwagger
    :<|> return graphSwagger
    :<|> genericServer nodeAPI
    :<|> genericServer linkAPI
  where
    nodeAPI :: NodeAPI AsServer
    nodeAPI =
      NodeAPI
        { list = return [Node {nodeId = NodeId 10, nodeLabel = "wat"}],
          create = \NodeReq {nrLabel = nl} -> return $ Node {nodeId = NodeId 100, nodeLabel = nl},
          delete = \_id -> return NoContent,
          rename = \_id NodeReq {nrLabel = nl} -> return $ Node {nodeId = NodeId 42, nodeLabel = nl},
          neighbours = \_id -> return [Node (NodeId 14) "Hurr", Node (NodeId 15) "Durr"]
        }
    linkAPI :: LinkAPI AsServer
    linkAPI = LinkAPI {new = \_fro _to -> return NoContent}

api :: Proxy API
api = Proxy

waiApp :: Connection -> Application
waiApp conn = do
  serve api $ server conn
