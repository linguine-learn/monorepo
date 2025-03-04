module Linguine.Auth.EmailPassword.Register (emailPasswordRegisterHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2 (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Data.Pool (Pool)
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Linguine.Auth (setSessionCookie)
import Linguine.Models.Session (createSession, generateSessionToken)
import Linguine.Models.User qualified as M
import Network.HTTP.Types (badRequest400, internalServerError500)
import Web.Scotty (ActionM, json, jsonData, status)
import Text.Email.Validate (isValid)
import Data.ByteString.UTF8 qualified as BSU

data RegisterData = RegisterData
  { email :: String,
    password :: String,
    retypedPassword :: String
  }
  deriving (Generic, FromJSON)

data RegisterResponse = RegisterResponse
  { success :: Bool,
    message :: String
  }
  deriving (Generic, ToJSON)

emailPasswordRegisterHandler :: Pool Connection -> ActionM ()
emailPasswordRegisterHandler pool = do
  registerData <- jsonData

  let uEmail = email registerData
      uPassword = password registerData
      uRetypedPassword = retypedPassword registerData

  case (uEmail, uPassword) of
    ("", _) -> do
      status badRequest400
      json RegisterResponse {success = False, message = "Invalid or missing fields."}
    (_, "") -> do
      status badRequest400
      json RegisterResponse {success = False, message = "Invalid or missing fields."}
    _ | uPassword /= uRetypedPassword -> do
      status badRequest400
      json RegisterResponse {success = False, message = "Passwords don't match."}
    _ | not $ isValid(BSU.fromString uEmail) -> do
      status badRequest400
      json RegisterResponse {success = False, message = "Invalid or missing fields."}
    -- TODO: Investigate password strength or handle in Client SDK
    -- TODO: Verification emails
    _  -> do
      maybeUser <- liftIO $ M.getUserByEmail pool uEmail
      case maybeUser of
        Just _ -> do
          status badRequest400
          json RegisterResponse {success = False, message = "Email is already in use"}
        Nothing -> do
          hashedPassword <- hashPassword $ mkPassword $ T.pack uPassword
          let hashedPasswordText = T.unpack $ unPasswordHash hashedPassword
          maybeCreatedUser <- liftIO $ M.createUser pool M.UserCreate {M.createPassword = Just hashedPasswordText, M.createEmail = uEmail}
          case maybeCreatedUser of
            Just user -> do
              sessionToken <- liftIO generateSessionToken
              session <- liftIO $ createSession pool sessionToken (M.userId user)
              setSessionCookie session
              json RegisterResponse {success = True, message = "Successfully created user!"}
            Nothing -> do
              status internalServerError500
              json RegisterResponse {success = False, message = "Failed to create user."}
