{-# LANGUAGE OverloadedStrings #-}

module Linguine.DB.Mutations where

import Database.PostgreSQL.Simple (Connection, execute)
import GHC.Generics (Generic)

data CreateUser = CreateUser {
  user_email :: String,
  user_name :: String,
  user_password :: String
} deriving (Show, Generic)

createUser :: Connection -> CreateUser -> IO ()
createUser conn user = do
  _ <- execute conn "INSERT INTO users (email, username, password) VALUES (?, ?, ?)" [user_email user, user_name user, user_password user]
  pure ()
