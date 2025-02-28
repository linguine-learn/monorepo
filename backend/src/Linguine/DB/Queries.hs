{-# LANGUAGE OverloadedStrings #-}

module Linguine.DB.Queries (getUserByEmail, getUserById) where

import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Linguine.DB.Models qualified as M

getUserByEmail :: Connection -> String -> IO [M.User]
getUserByEmail conn email = do
  query conn "SELECT * FROM users WHERE email = ?" (Only email) :: IO [M.User]

getUserById :: Connection -> Int -> IO [M.User]
getUserById conn userId = do
  query conn "SELECT * FROM users WHERE id = ?" (Only userId) :: IO [M.User]
