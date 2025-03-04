module Linguine.Auth.EmailPassword.Login (emailPasswordLoginHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2 (PasswordCheck (PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, mkPassword)
import Data.Pool (Pool)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Linguine.Auth (setSessionCookie)
import Linguine.Models.Session (createSession, generateSessionToken)
import Linguine.Models.User qualified as M
import Network.HTTP.Types (badRequest400)
import Web.Scotty (ActionM, json, jsonData, status)

data LoginData = LoginData
  { email :: String,
    password :: String
  }
  deriving (Generic, FromJSON)

data LoginResponse = LoginResponse
  { success :: Bool,
    message :: String
  }
  deriving (Generic, ToJSON)

emailPasswordLoginHandler :: Pool Connection -> ActionM ()
emailPasswordLoginHandler pool = do
  loginData <- jsonData
  let uEmail = email loginData
      uPassword = password loginData

  case (uEmail, uPassword) of
    ("", _) -> do
      status badRequest400
      json LoginResponse {success = False, message = "Invalid or missing fields."}
    (_, "") -> do
      status badRequest400
      json LoginResponse {success = False, message = "Invalid or missing fields."}
    _ -> do
      maybeUser <- liftIO $ M.getUserByEmail pool uEmail
      case maybeUser of
        Nothing -> do
          status badRequest400
          json LoginResponse {success = False, message = "Invalid email or password."}
        Just user -> do
          let attemptedPassword = mkPassword $ T.pack uPassword
              maybePassword = M.password user
              passwordsMatch = maybePassword >>= \pass -> Just $ checkPassword attemptedPassword $ PasswordHash {unPasswordHash = T.pack pass}
          case passwordsMatch of
            Just PasswordCheckSuccess -> do
              sessionToken <- liftIO generateSessionToken
              session <- liftIO $ createSession pool sessionToken (M.userId user)
              setSessionCookie session
              json LoginResponse {success = True, message = "Success!"}
            _ -> do
              status badRequest400
              json LoginResponse {success = False, message = "Invalid email or password."}
