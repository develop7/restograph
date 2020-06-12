{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( waiApp,
  )
where

import Control.Lens ((&), (.~), (?~), mapped)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Swagger (Swagger (..), ToParamSchema, ToSchema (..), defaultSchemaOptions, description, example, genericDeclareNamedSchema, info, schema, title, version)
import Database.PostgreSQL.LibPQ (Connection)
import Deriving.Aeson.Stock
import Hasql.Connection (Settings, withLibPQConnection)
import Hasql.Pool (Pool, acquire, use)
import Hasql.Session (run)
import Network.Wai (Application)
import Restograph.Database.Sessions
import Restograph.Model (Node (..), NodeId (..), NodeReq (..))
import Servant (throwError)
import Servant.API ((:<|>) (..), (:<|>), (:>), Capture, DeleteNoContent, FromHttpApiData, Get, JSON, NoContent (..), PlainText, Put, PutNoContent, ReqBody)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Server (Handler, Server, err404, err412, err500, errBody, serve)
import Servant.Server.Generic (AsServer, genericServer)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.Vector (toList)

deriving instance FromHttpApiData NodeId

instance ToParamSchema NodeId

instance ToSchema NodeId

instance ToSchema Node where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Graph node"
      & mapped . schema . example ?~ toJSON (Node (NodeId 42) "answer")

instance ToSchema NodeReq where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Node-related request data"
      & mapped . schema . example ?~ toJSON (NodeReq "answer")

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> SwaggerAPI :<|> GraphAPI

type GraphAPI = "graph" :> ("node" :> ToServantApi NodeAPI :<|> "link" :> ToServantApi LinkAPI)

data NodeAPI r
  = NodeAPI
      { create :: r :- ReqBody '[JSON] NodeReq :> Put '[JSON] Int32,
        delete :: r :- Capture "id" NodeId :> DeleteNoContent '[PlainText] NoContent,
        rename :: r :- Capture "id " NodeId :> "label" :> ReqBody '[JSON] NodeReq :> Put '[JSON] NoContent,
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

server :: Pool -> Server API
server pool =
  swaggerSchemaUIServer graphSwagger
    :<|> return graphSwagger
    :<|> genericServer nodeAPI
    :<|> genericServer linkAPI
  where
    nodeAPI :: NodeAPI AsServer
    nodeAPI =
      NodeAPI
        { list,
          create,
          delete,
          rename,
          neighbours
        }
    list :: Handler [Node]
    list = do
      result <- liftIO $ use pool listNodes
      case result of
        Right nodes -> return nodes
        Left _e -> throwError err500
    create :: NodeReq -> Handler Int32
    create NodeReq {nrLabel} = do
      result <- liftIO $ use pool $ createNode nrLabel
      case result of
        Right mnid ->
          case mnid of
            Just nid -> return nid
            Nothing -> throwError err500
        Left _e -> throwError err500
    delete :: NodeId -> Handler NoContent
    delete (NodeId nid) = do
      result <- liftIO $ use pool $ deleteNode nid
      case result of
        Right r ->
          if r
            then return NoContent
            else throwError $ err404 {errBody = pack $ "Couldn't find node with ID = " ++ show nid}
        Left _e -> throwError err500
    rename :: NodeId -> NodeReq -> Handler NoContent
    rename (NodeId nid) NodeReq {nrLabel} = do
      result <- liftIO $ use pool $ renameNode nid nrLabel
      case result of
        Right r ->
          if r
            then return NoContent
            else throwError err404 {errBody = pack $ "Couldn't find node with ID = " ++ show nid}
        Left _e -> throwError err500
    neighbours :: NodeId -> Handler [Node]
    neighbours (NodeId nid) = do
      result <- liftIO $ use pool $ listNodeNeighbors nid
      case result of 
        Right nodes -> return $ toList nodes
        Left _e -> throwError err500
    linkAPI :: LinkAPI AsServer
    linkAPI = LinkAPI {new}
    new :: NodeId -> NodeId -> Handler NoContent
    new (NodeId fro) (NodeId to)
      | fro == to = throwError (err412 {errBody = "Cannot link node to itself"})
      | otherwise = do
        result <- liftIO $ use pool $ linkNodes fro to
        case result of
          Right r -> if r then return NoContent else throwError $ err404 {errBody = "Couldn't find requested node"}
          Left _e -> throwError err500

api :: Proxy API
api = Proxy

waiApp :: Pool -> Application
waiApp pool = do
  serve api $ server pool
