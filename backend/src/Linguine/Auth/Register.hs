{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Linguine.Auth.Register (registerApi, RegisterAPI) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2
import Data.Pool (Pool, withResource)
import Data.Text (pack, unpack)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Linguine.DB.Mutations (CreateUser (CreateUser))
import Linguine.DB.Mutations qualified as DBM
import Linguine.DB.Queries qualified as DBQ
import Servant (Handler, JSON, ReqBody, Server, StdMethod (POST), UVerb, Union, WithStatus (WithStatus), respond, (:>))
import Text.Regex.TDFA

data RegisterResult = RegisterResult
  { message :: String
  }
  deriving (Show, Generic, ToJSON)

data RegisterData = RegisterData
  { username :: String,
    email :: String,
    password :: String,
    retypedPassword :: String
  }
  deriving (Show, Generic, FromJSON)

type RegisterAPI = "auth" :> "register" :> ReqBody '[JSON] RegisterData :> UVerb 'POST '[JSON] '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult, WithStatus 500 RegisterResult]

registerUser :: Pool Connection -> RegisterData -> Handler (Union '[WithStatus 200 RegisterResult, WithStatus 201 RegisterResult, WithStatus 500 RegisterResult])
registerUser connectionPool registerData = do
  let emailRegex = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$" :: String
      registerDataErrors =
        [ (email registerData == "", "Email must not be empty!" :: String),
          ((length $ email registerData) > 255, "Please enter a valid email!"),
          (not $ (email registerData) =~ emailRegex :: Bool, "Please enter a valid email!"),
          (password registerData == "", "Password must not be empty!"),
          ((length $ password registerData) < 8, "Password must be at least 8 characters!"),
          ((password registerData /= retypedPassword registerData), "Passwords don't match!"),
          (username registerData == "", "Username must not be empty!"),
          ((length $ username registerData) > 25, "Username must be less than 25 characters!")
        ]

  case lookup True registerDataErrors of
    Just errorMessage -> respond $ WithStatus @200 RegisterResult {message = errorMessage}
    Nothing -> do
      users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserByEmail conn (email registerData)
      case users of
        [] -> do
          let passwordHashingParams =
                Argon2Params
                  { argon2Salt = 16,
                    argon2Variant = Argon2id,
                    argon2Version = Version13,
                    argon2TimeCost = 3,
                    argon2Parallelism = 1,
                    argon2MemoryCost = 12288,
                    argon2OutputLength = 32
                  }

          hashedPassword <- hashPasswordWithParams passwordHashingParams $ mkPassword (pack $ password registerData)

          let createUserData =
                CreateUser
                  { user_email = email registerData,
                    user_name = username registerData,
                    user_password = unpack $ unPasswordHash hashedPassword
                  }

          liftIO $ withResource connectionPool $ \conn -> DBM.createUser conn createUserData
          respond $ WithStatus @201 RegisterResult {message = "Success!"}

        -- TODO: Send verification email so this can be avoided.
        [_] -> respond $ WithStatus @200 RegisterResult {message = "Email already in use!"}
        _ -> respond $ WithStatus @500 RegisterResult {message = "Unkown error occured."}

registerApi :: Pool Connection -> Server RegisterAPI
registerApi connectionPool = registerUser connectionPool
