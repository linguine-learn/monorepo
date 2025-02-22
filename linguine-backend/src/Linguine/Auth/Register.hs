{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Linguine.Auth.Register (registerApi, RegisterAPI) where

import qualified Linguine.DB.Queries as DBQ
import qualified Linguine.DB.Mutations as DBM

import Servant ((:>), ReqBody, JSON, StdMethod(POST), Handler, Server, UVerb, WithStatus (WithStatus), Union, respond)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Linguine.DB.Mutations (CreateUser(CreateUser))

data RegisterResult = RegisterResult {
  message :: String
} deriving (Show, Generic, ToJSON)

data RegisterData = RegisterData {
  name :: String, 
  email :: String, 
  password ::String, 
  retypedPassword ::String
} deriving (Show, Generic, FromJSON)

type RegisterAPI = "auth" :> "register" :> ReqBody '[JSON] RegisterData :> UVerb 'POST '[JSON] '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult]

registerUser :: Pool Connection -> RegisterData -> Handler (Union '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult])
registerUser conns registerData = do
  if (password registerData) == ""
    then respond $ WithStatus @200 RegisterResult { message = "Password must not be empty!" }
  else if (password registerData) /= (retypedPassword registerData)
    then respond $ WithStatus @200 RegisterResult { message = "Passwords don't match!" }
  else if (email registerData) == ""
    then respond $ WithStatus @200 RegisterResult { message = "Email must not be empty!" }
  else if not $ '@' `elem` (email registerData)
    then respond $ WithStatus @200 RegisterResult { message = "Please enter a valid email!" }
  else do
    users <- liftIO $ withResource conns $ \conn -> DBQ.getUserByEmail conn (email registerData)
    case users of
      [] -> do
        let createUserData = CreateUser {
          user_email = email registerData,
          user_name = name registerData,
          -- TODO: Encrypt password using argon2id
          user_password = password registerData
        }
        
        liftIO $ withResource conns $ \conn -> DBM.createUser conn createUserData
        respond $ WithStatus @201 RegisterResult { message = "Success!" }

      -- TODO: Send verification email so this can be avoided.
      [_] -> respond $ WithStatus @200 RegisterResult { message = "Email already in use!" }
      _ -> respond $ WithStatus @200 RegisterResult { message = "Unkown error occured." }

registerApi :: Pool Connection -> Server RegisterAPI
registerApi conns = registerUser conns
