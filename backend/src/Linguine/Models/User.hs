module Linguine.Models.User (User (..), UserCreate(..), getUserByEmail, createUser) where

import Data.Pool (Pool, withResource)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), query)
import GHC.Generics (Generic)

data User = User
  { userId :: Int,
    email :: String,
    password :: Maybe String,
    createdAt :: UTCTime
  }
  deriving (Generic, FromRow)

data UserCreate = UserCreate
  { createEmail :: String,
    createPassword :: Maybe String
  }

getUserByEmail :: Pool Connection -> String -> IO (Maybe User)
getUserByEmail pool email = do
  withResource pool $ \conn -> do
    users <- query conn "SELECT id, email, password, created_at FROM users WHERE email = ?" (Only email) :: IO [User]
    case users of
      [user] -> pure $ Just user
      _ -> pure Nothing

createUser :: Pool Connection -> UserCreate -> IO (Maybe User)
createUser pool userData = do
  withResource pool $ \conn -> do
    users <-  query conn " INSERT INTO users (email, password) VALUES (?, ?) RETURNING id, email, password, created_at" (createEmail userData, createPassword userData) :: IO [User]
    case users of
      [user] -> pure $ Just user
      _ -> pure Nothing
