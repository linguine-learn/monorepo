{-# LANGUAGE OverloadedStrings #-}

module Linguine.Server where

import Linguine.Auth.Register (RegisterAPI, registerApi)
import Linguine.Auth.Login (loginApi, LoginAPI)

import Network.Wai.Handler.Warp (run)

import Data.Pool (Pool, newPool, defaultPoolConfig)
import Data.ByteString.Lazy.Char8 (pack, toStrict)
import Data.Data (Proxy(Proxy))

import Database.PostgreSQL.Simple (connectPostgreSQL, close, Connection)

import Control.Monad.IO.Class (MonadIO(liftIO))

import Configuration.Dotenv (loadFile, defaultConfig)
import Configuration.Dotenv.Environment (lookupEnv)

import Servant.Server (Server, Application, serve)
import Servant  ((:<|>)(..))
import Linguine.Auth.Refresh (RefreshAPI, refreshApi)

type LinguineAPI = RegisterAPI :<|> LoginAPI :<|> RefreshAPI

linguineApi :: Pool Connection -> Server LinguineAPI
linguineApi connectionPool = do
  registerApi connectionPool :<|> loginApi connectionPool :<|> refreshApi connectionPool

app :: Pool Connection -> Application
app connectionPool = serve (Proxy :: Proxy LinguineAPI) $ linguineApi connectionPool

serveLinguine :: IO ()
serveLinguine = do
  loadFile defaultConfig
  maybeConnectionPooltr <- lookupEnv "POSTGRES_URI"
  maybeAccessSecret <- lookupEnv "ACCESS_SECRET"
  maybeRefreshSecert <- lookupEnv "REFRESH_SECRET"

  case (maybeConnectionPooltr, maybeAccessSecret, maybeRefreshSecert) of
    (Just connectionPooltr, Just _, Just _) -> do
      pool <- liftIO $ newPool $ defaultPoolConfig (connectPostgreSQL $ toStrict (pack connectionPooltr)) close 60 10
      putStrLn "Running on port 3000"
      run 3000 (app pool)
    (Nothing, _, _) ->
      putStrLn "FAILED TO LOAD POSTGRES_URI"
    (_, Nothing, _) ->
      putStrLn "FAILED TO LOAD ACCESS_SECRET"
    (_, _, Nothing) ->
      putStrLn "FAILED TO LOAD REFRESH_SECRET"
