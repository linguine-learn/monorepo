{-# LANGUAGE OverloadedStrings #-}
module Linguine.DB.Queries (getUserByEmail) where

import Database.PostgreSQL.Simple (Connection, Only(Only), query)
import qualified Linguine.DB.Models as M

getUserByEmail :: Connection -> String -> IO [M.User]
getUserByEmail conn email = do
  query conn "SELECT * FROM users WHERE email = ?" (Only email) :: IO [M.User]
