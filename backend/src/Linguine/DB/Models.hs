{-# LANGUAGE DeriveAnyClass #-}

module Linguine.DB.Models where

import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

data User = User
  { user_id :: Int,
    user_email :: String,
    user_name :: String,
    user_password :: String,
    user_refreshTokenVersion :: Int
  }
  deriving (Show, Generic, FromRow)
