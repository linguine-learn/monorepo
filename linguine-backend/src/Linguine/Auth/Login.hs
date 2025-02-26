{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

{- TODO: Make auth flow more robust by using throttling -}

module Linguine.Auth.Login (loginApi, LoginAPI, makeRefreshCookie) where

import qualified Linguine.DB.Queries as DBQ
import qualified Data.ByteString.Char8 as BSC

import Servant 

import GHC.Generics (Generic)

import Data.Pool (Pool, withResource)
import Data.Aeson (ToJSON, FromJSON)

import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Password.Argon2
import Linguine.DB.Models (User(user_password, user_id, user_refreshTokenVersion))
import Data.Text (pack)
import Linguine.Auth.JWT (makeJwtPair)
import Web.Cookie 
import Data.Time (UTCTime)

data LoginResult = LoginResult {
  message :: String,
  token :: Maybe String
} deriving (Show, Generic, ToJSON)

data LoginData  = LoginData {
  email :: String,
  password :: String
} deriving (Show, Generic, FromJSON)

type LoginAPI = "auth" :> "login" :> 
    ReqBody '[JSON] LoginData :>
        UVerb 'POST  '[JSON] '[WithStatus 200 (Headers '[Header "Set-Cookie" String] LoginResult),  WithStatus 500 LoginResult]

myNoHeader :: LoginResult ->  Headers '[Header "Set-Cookie" String] LoginResult
myNoHeader loginResult = noHeader loginResult

setRefreshCookie :: String -> LoginResult ->  Headers '[Header "Set-Cookie" String] LoginResult
setRefreshCookie refreshToken loginResult = addHeader refreshToken loginResult

makeRefreshCookie :: String -> UTCTime-> String
makeRefreshCookie refreshToken expiryTime = do
  let refreshCookieOptions = defaultSetCookie {
    setCookieName = "refreshToken",
    setCookieValue = BSC.pack refreshToken,
    setCookiePath = Just "/",
    setCookieHttpOnly = True,
    setCookieExpires = Just expiryTime,

    -- TODO: toggle between True and False depending on environment
    setCookieSecure = False
  }
  BSC.unpack $ renderSetCookieBS refreshCookieOptions

loginUser :: Pool Connection -> LoginData -> Handler (Union '[WithStatus 200 (Headers '[Header "Set-Cookie" String] LoginResult), WithStatus 500 LoginResult])
loginUser connectionPool loginData = do
  if (email loginData) == ""
    then respond $ WithStatus @200 $ myNoHeader LoginResult { message = "Email must not be empty!", token = Nothing }
  else if (password loginData) == ""
    then respond $ WithStatus @200 $ myNoHeader LoginResult { message = "Password must not be empty!", token = Nothing }
  else do
    users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserByEmail conn (email loginData)
    case users of
      [user] -> do
        let passwordCheck = checkPassword (mkPassword $ pack $ password loginData) (PasswordHash $ pack $ user_password user)
        if passwordCheck == PasswordCheckFail
          then respond $ WithStatus @200 $ myNoHeader LoginResult { message = "Invalid username or password.", token = Nothing }
        else if passwordCheck == PasswordCheckSuccess
          then do
            (accessToken, refreshToken, refreshTokenExpiryTime) <- liftIO $ makeJwtPair (user_id user, user_refreshTokenVersion user)
            let refreshCookie = makeRefreshCookie refreshToken refreshTokenExpiryTime

            respond $ WithStatus @200 $ setRefreshCookie refreshCookie LoginResult { message = "Success", token = Just accessToken }
        else do
          respond $ WithStatus @500 LoginResult { message = "Unkown error occured.", token = Nothing }

      [] -> respond $ WithStatus @200 $ myNoHeader LoginResult { message = "Invalid username or password.", token = Nothing }
      _ -> respond $ WithStatus @500 LoginResult { message = "Unkown error occured.", token = Nothing }

loginApi :: Pool Connection -> Server LoginAPI
loginApi connectionPool = loginUser connectionPool
