{-# LANGUAGE OverloadedStrings #-}

module Linguine.Server where

import Configuration.Dotenv (defaultConfig, loadFile)
import Configuration.Dotenv.Environment (lookupEnv)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (pack, toStrict)
import Data.Data (Proxy (Proxy))
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Linguine.Auth.Login (LoginAPI, loginApi)
import Linguine.Auth.Refresh (RefreshAPI, refreshApi)
import Linguine.Auth.Register (RegisterAPI, registerApi)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>) (..))
import Servant.Server (Application, Server, serve)

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
