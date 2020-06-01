{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( waiApp,
  )
where

import Control.Lens ((&), (.~), (?~), mapped)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.Aeson as Aeson (Value (..))
import Data.Proxy (Proxy (..))
import Data.Swagger (NamedSchema (..), Swagger (..), SwaggerItems (..), SwaggerType (..), ToSchema (..), defaultSchemaOptions, description, example, genericDeclareNamedSchema, info, items, schema, title, type_, version)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.LibPQ (Connection)
import Deriving.Aeson.Stock
import GHC.Generics
import Hasql.Connection ()
import Network.Wai (Application)
import Servant.API ((:<|>) (..), (:<|>), (:>), Accept, Capture, Delete, DeleteNoContent, Get, JSON, NoContent (..), PlainText, Put, PutNoContent, ReqBody)
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant.Server (Server, serve)
import Servant.Server.Generic (AsServer, genericServer)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

data Node = Node {nodeId :: Int, nodeLabel :: Text}
  deriving (Generic, Typeable)
  deriving (FromJSON, ToJSON) via PrefixedSnake "node" Node

instance ToSchema Node where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Graph node"
      & mapped . schema . example ?~ toJSON (Node 42 "answer")

data NodeReq = NodeReq {nrLabel :: Text}
  deriving (Generic, Typeable)
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
        delete :: r :- Capture "id" Int :> DeleteNoContent '[PlainText] NoContent,
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

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

graphSwagger :: Swagger
graphSwagger =
  toSwagger (Proxy :: Proxy GraphAPI)
    & info . title .~ "Graph manipulation API"
    & info . version .~ "0.1"
    & info . description ?~ "An undirected graph manipulation API"

server :: Connection -> Server API
server conn =
  swaggerSchemaUIServer graphSwagger 
    :<|> return graphSwagger
    :<|> genericServer nodeAPI
    :<|> genericServer linkAPI
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
    linkAPI = LinkAPI {new = \fro to -> return NoContent}

api :: Proxy API
api = Proxy

waiApp :: Connection -> Application
waiApp conn = do
  serve api $ server conn
