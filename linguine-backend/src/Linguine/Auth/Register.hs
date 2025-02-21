{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Linguine.Auth.Register (registerApi, RegisterAPI) where

import Servant
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics 

data RegisterResult = RegisterResult {
  message :: String
} deriving (Show, Generic, ToJSON)

data RegisterData = RegisterData {
  name :: String, 
  email :: String, 
  password ::String, 
  retypedPassword ::String
} deriving (Show, Generic, FromJSON)

type RegisterAPI = "auth" :> "register" :> ReqBody '[JSON] RegisterData :> Post '[JSON] RegisterResult

registerUser :: RegisterData -> Handler RegisterResult
registerUser registerData = do
  if (password registerData) /= (retypedPassword registerData)
    then return RegisterResult { message = "Passwords don't match!" }
  else do
    -- investigate sending verifcation emails
    -- postgres connection pooling to add user to the database
    undefined

registerApi :: Server RegisterAPI
registerApi = (registerUser)
