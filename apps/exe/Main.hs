module Main where

import Lib

main :: IO ()
main = do
  port <- fromMaybe "3000" <$> lookupEnv "PORT"
  pgURI <- fromMaybe "postgres:///restograph" <$> lookupEnv "DATABASE_URL"
  
  runApp (read port) pgURI 
