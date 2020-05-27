module App (runApp) where

import Lib

import Network.Wai.Handler.Warp (run)


runApp :: Int -> String -> IO()
runApp port _pgURI = do
--  conn <- connectPostgreSQL pgURI
  run port 
  ()
