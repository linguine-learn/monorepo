{-# LANGUAGE OverloadedStrings #-}

module Linguine.Server where

import Linguine.Auth.Register (RegisterAPI, registerApi)
import Servant.Server
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Data.Pool (Pool, newPool, defaultPoolConfig)
import Database.PostgreSQL.Simple (connectPostgreSQL, close, Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Configuration.Dotenv (loadFile, defaultConfig)
import Configuration.Dotenv.Environment (lookupEnv)
import Data.ByteString.Lazy.Char8 (pack, toStrict)

type LinguineAPI = RegisterAPI

linguineApi :: Pool Connection -> Server LinguineAPI
linguineApi conns = do
  registerApi conns

app :: Pool Connection -> Application
app conns = serve (Proxy :: Proxy LinguineAPI) $ linguineApi conns

serveLinguine :: IO ()
serveLinguine = do
  loadFile defaultConfig
  maybeConnStr <- lookupEnv "POSTGRES_URI"

  case maybeConnStr of
    Just connStr -> do
      pool <- liftIO $ newPool $ defaultPoolConfig (connectPostgreSQL $ toStrict (pack connStr)) close 60 10
      putStrLn "Running on port 3000"
      run 3000 (app pool)
    Nothing ->
      putStrLn "FAILED TO LOAD POSTGRES_URI"
