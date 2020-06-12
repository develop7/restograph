module Main where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Lib
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Hasql.Pool (acquire)

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
  pool <- acquire (1, 1, pack pgURI)
  run port (middleware $ waiApp pool)
  pure ()
