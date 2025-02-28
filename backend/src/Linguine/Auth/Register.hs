{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Linguine.Auth.Register (registerApi, RegisterAPI) where

import qualified Linguine.DB.Queries as DBQ
import qualified Linguine.DB.Mutations as DBM
import Linguine.DB.Mutations (CreateUser(CreateUser))

import Servant ((:>), ReqBody, JSON, StdMethod(POST), Handler, Server, UVerb, WithStatus (WithStatus), Union, respond)

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Data.Pool (Pool, withResource)

import Database.PostgreSQL.Simple (Connection)

import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Password.Argon2 
import Data.Text (pack, unpack)

data RegisterResult = RegisterResult {
  message :: String
} deriving (Show, Generic, ToJSON)

data RegisterData = RegisterData {
  username :: String, 
  email :: String, 
  password ::String, 
  retypedPassword ::String
} deriving (Show, Generic, FromJSON)

type RegisterAPI = "auth" :> "register" :> ReqBody '[JSON] RegisterData :> UVerb 'POST '[JSON] '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult, WithStatus 500 RegisterResult]

registerUser :: Pool Connection -> RegisterData -> Handler (Union '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult, WithStatus 500 RegisterResult])
registerUser connectionPool registerData = do
  if (email registerData) == ""
    then respond $ WithStatus @200 RegisterResult { message = "Email must not be empty!" }
  else if length (email registerData) > 255
    then respond $ WithStatus @200 RegisterResult { message = "Please enter a valid email!" }
  else if not $ '@' `elem` (email registerData)
    then respond $ WithStatus @200 RegisterResult { message = "Please enter a valid email!" }
  else if (password registerData) == ""
    then respond $ WithStatus @200 RegisterResult { message = "Password must not be empty!" }
  else if length (password registerData) < 8
    then respond $ WithStatus @200 RegisterResult { message = "Password must be at least 8 characters!" }
  else if (password registerData) /= (retypedPassword registerData)
    then respond $ WithStatus @200 RegisterResult { message = "Passwords don't match!" }
  else if (username registerData) == ""
    then respond $ WithStatus @200 RegisterResult { message = "Username must not be empty!" }
  else if length (username registerData) > 25
    then respond $ WithStatus @200 RegisterResult { message = "Username must be less than 25 characters!" }
  else do
    users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserByEmail conn (email registerData)
    case users of
      [] -> do
        let passwordHashingParams = Argon2Params {
          argon2Salt = 16,
          argon2Variant = Argon2id,
          argon2Version = Version13,
          argon2TimeCost = 3,
          argon2Parallelism = 1,
          argon2MemoryCost = 12288,
          argon2OutputLength = 32
        }

        hashedPassword <- hashPasswordWithParams passwordHashingParams $ mkPassword (pack $ password registerData)

        let createUserData = CreateUser {
          user_email = email registerData,
          user_name = username registerData,
          user_password = unpack $ unPasswordHash hashedPassword
        }
        
        liftIO $ withResource connectionPool $ \conn -> DBM.createUser conn createUserData
        respond $ WithStatus @201 RegisterResult { message = "Success!" }

      -- TODO: Send verification email so this can be avoided.
      [_] -> respond $ WithStatus @200 RegisterResult { message = "Email already in use!" }
      _ -> respond $ WithStatus @500 RegisterResult { message = "Unkown error occured." }

registerApi :: Pool Connection -> Server RegisterAPI
registerApi connectionPool = registerUser connectionPool
