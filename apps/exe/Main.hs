module Main where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.LibPQ (connectdb)
import Lib
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- fromMaybe "3000" <$> lookupEnv "PORT"
  env <- fromMaybe "development" <$> lookupEnv "ENV"
  pgURI <- fromMaybe "postgres:///restograph" <$> lookupEnv "DATABASE_URL"
  let middleware =
        case env of
          "development" -> logStdoutDev
          _ -> id
  runApp (read port) pgURI middleware

runApp :: Int -> String -> Middleware -> IO ()
runApp port pgURI middleware = do
  conn <- connectdb $ pack pgURI
  run port (middleware $ waiApp conn)
  pure ()
