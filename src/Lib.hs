{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( someFunc,
  )
where

import Data.Text (Text)
import GHC.Generics
import Servant.API ((:>), Capture, Delete, DeleteNoContent, Get, JSON, NoContent, Put, PutNoContent, ReqBody, (:<|>))
import Servant.API.Generic ((:-), ToServantApi)

data Node = Node {id :: Int, nodeLabel :: Text} deriving (Generic)

data NodeReq = NodeReq {nrLabel :: Text} deriving (Generic)

type API = "graph" :> GraphAPI

type GraphAPI = "node" :> ToServantApi NodeAPI :<|> "link" :> ToServantApi LinkAPI 

data NodeAPI route
  = NodeAPI
      { create :: route :- ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        delete :: route :- Capture "id" Int :> DeleteNoContent '[JSON] NoContent,
        rename :: route :- Capture "id " Int :> "label" :> ReqBody '[JSON] NodeReq :> Put '[JSON] Node,
        neighbours :: route :- Capture "id " Int :> "neighbours" :> Get '[JSON] [Node]
      }

data LinkAPI route
  = LinkAPI
      { new :: Capture "id_from" Int :> Capture "id_to" Int :> PutNoContent '[NoContent] NoContent
      }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
