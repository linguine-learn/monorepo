{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Linguine.Auth.Login (loginApi, LoginAPI) where

import qualified Linguine.DB.Queries as DBQ

import Servant ((:>), ReqBody, JSON, StdMethod(POST), Handler, Server, UVerb, WithStatus (WithStatus), Union, respond)

import GHC.Generics (Generic)

import Data.Pool (Pool, withResource)
import Data.Aeson (ToJSON, FromJSON)

import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Password.Argon2 (checkPassword, mkPassword, PasswordHash (PasswordHash), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess))
import Linguine.DB.Models (User(user_password))
import Data.Text (pack)


data LoginResult = LoginResult {
  message :: String
} deriving (Show, Generic, ToJSON)

data LoginData  = LoginData {
  email :: String,
  password :: String
} deriving (Show, Generic, FromJSON)

type LoginAPI = "auth" :> "login" :> ReqBody '[JSON] LoginData :> UVerb 'POST  '[JSON] '[WithStatus 200 LoginResult, WithStatus 500 LoginResult]

loginUser :: Pool Connection -> LoginData -> Handler (Union '[WithStatus 200 LoginResult, WithStatus 500 LoginResult])
loginUser connectionPool loginData = do
  if (email loginData) == ""
    then respond $ WithStatus @200 LoginResult { message = "Email must not be empty!" }
  else if (password loginData) == ""
    then respond $ WithStatus @200 LoginResult { message = "Password must not be empty!" }
  else do
    users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserByEmail conn (email loginData)
    case users of
      [user] -> do
        let passwordCheck = checkPassword (mkPassword $ pack $ password loginData) (PasswordHash $ pack $ user_password user)
        if passwordCheck == PasswordCheckFail
          then respond $ WithStatus @200 LoginResult { message = "Invalid username or password." }
        else if passwordCheck == PasswordCheckSuccess
          then do
            -- TODO: Generate JWT token pair
            respond $ WithStatus @200 LoginResult { message = "Success" }
        else do
          respond $ WithStatus @500 LoginResult { message = "Unkown error occured." }

      [] -> respond $ WithStatus @200 LoginResult { message = "Invalid username or password." }
      _ -> respond $ WithStatus @500 LoginResult { message = "Unkown error occured." }

loginApi :: Pool Connection -> Server LoginAPI
loginApi connectionPool = loginUser connectionPool
