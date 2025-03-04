module Linguine.Server (serveLinguine) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (forM_)
import Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.Vault.Lazy qualified as V
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Linguine.Auth.Middleware (authMiddleware)
import Linguine.Auth.OAuth.Google (googleOAuthServer)
import Linguine.Models.Session (ValidSession)
import System.Environment (getEnv, lookupEnv)
import System.Exit (exitFailure)
import Web.Scotty

authenticatedRoutes :: V.Key ValidSession -> Pool Connection -> ScottyM ()
authenticatedRoutes key pool = do
  middleware $ authMiddleware key pool

publicRoutes :: Pool Connection -> ScottyM ()
publicRoutes pool =
  googleOAuthServer pool

serveLinguine :: IO ()
serveLinguine = do
  loadFile defaultConfig
  let requiredVariables :: [String] = ["POSTGRES_URI", "BASE_URL"]
  forM_ requiredVariables $ \variableName -> do
    maybeValue <- lookupEnv variableName
    case maybeValue of
      Just value | value /= "" -> pure ()
      _ -> do
        putStrLn $ "Missing environment variable: " ++ variableName
        exitFailure
  postgresUri <- getEnv "POSTGRES_URI"
  postgresPool <- newPool $ defaultPoolConfig (connectPostgreSQL $ BSU.fromString postgresUri) close 60 10
  key <- V.newKey

  scotty 3000 $ authenticatedRoutes key postgresPool <> publicRoutes postgresPool
