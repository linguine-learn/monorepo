{-# LANGUAGE DeriveAnyClass #-}
module Linguine.DB.Models where

import GHC.Generics 
import Database.PostgreSQL.Simple (FromRow)

data User = User {
  user_id :: Int,
  user_email :: String,
  user_name :: String,
  user_password :: String,
  user_refreshTokenVersion :: Int
} deriving (Show, Generic, FromRow)
