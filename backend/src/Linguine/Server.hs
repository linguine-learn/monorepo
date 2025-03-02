module Linguine.Server (serveLinguine) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (forM_)
import Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Linguine.Auth.OAuth.Google (googleOAuthServer)
import System.Environment (getEnv, lookupEnv)
import System.Exit (exitFailure)
import Web.Scotty

authenticatedRoutes :: ScottyM ()
authenticatedRoutes = pure ()

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

  scotty 3000 $ authenticatedRoutes <> publicRoutes postgresPool
