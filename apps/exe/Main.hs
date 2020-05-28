module Main where

import Lib
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Database.PostgreSQL.LibPQ (connectdb)
import Data.ByteString.Char8 (pack)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  port <- fromMaybe "3000" <$> lookupEnv "PORT"
  pgURI <- fromMaybe "postgres:///restograph" <$> lookupEnv "DATABASE_URL"
  
  runApp (read port) pgURI

runApp :: Int -> String -> IO()
runApp port pgURI = do
  conn <- connectdb $ pack pgURI
  run port (waiApp conn)
  pure ()
