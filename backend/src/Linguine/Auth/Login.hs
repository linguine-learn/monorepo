{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{- TODO: Make auth flow more robust by using throttling -}

module Linguine.Auth.Login (loginApi, LoginAPI, makeRefreshCookie) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as BSC
import Data.Password.Argon2
import Data.Pool (Pool, withResource)
import Data.Text (pack)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Linguine.Auth.JWT (makeJwtPair)
import Linguine.DB.Models (User (user_id, user_password, user_refreshTokenVersion))
import Linguine.DB.Queries qualified as DBQ
import Servant
import Web.Cookie

data LoginResult = LoginResult
  { message :: String,
    token :: Maybe String
  }
  deriving (Show, Generic, ToJSON)

data LoginData = LoginData
  { email :: String,
    password :: String
  }
  deriving (Show, Generic, FromJSON)

type LoginAPI =
  "auth"
    :> "login"
    :> ReqBody '[JSON] LoginData
    :> UVerb 'POST '[JSON] '[WithStatus 200 (Headers '[Header "Set-Cookie" String] LoginResult), WithStatus 500 LoginResult]

myNoHeader :: LoginResult -> Headers '[Header "Set-Cookie" String] LoginResult
myNoHeader loginResult = noHeader loginResult

setRefreshCookie :: String -> LoginResult -> Headers '[Header "Set-Cookie" String] LoginResult
setRefreshCookie refreshToken loginResult = addHeader refreshToken loginResult

makeRefreshCookie :: String -> UTCTime -> String
makeRefreshCookie refreshToken expiryTime = do
  let refreshCookieOptions =
        defaultSetCookie
          { setCookieName = "refreshToken",
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
  let registerDataErrors =
        [ (email loginData == "", "Email must not be empty!" :: String),
          ((password loginData == ""), "Password must not be empty!")
        ]

  case lookup True registerDataErrors of
    Just errorMessage -> respond $ WithStatus @200 $ myNoHeader LoginResult {message = errorMessage, token = Nothing}
    Nothing -> do
      users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserByEmail conn (email loginData)
      case users of
        [user] -> do
          let passwordCheck = checkPassword (mkPassword $ pack $ password loginData) (PasswordHash $ pack $ user_password user)
          case passwordCheck of
            PasswordCheckFail -> respond $ WithStatus @200 $ myNoHeader LoginResult {message = "Invalid username or password.", token = Nothing}
            PasswordCheckSuccess -> do
              (accessToken, refreshToken, refreshTokenExpiryTime) <- liftIO $ makeJwtPair (user_id user, user_refreshTokenVersion user)
              let refreshCookie = makeRefreshCookie refreshToken refreshTokenExpiryTime

              respond $ WithStatus @200 $ setRefreshCookie refreshCookie LoginResult {message = "Success", token = Just accessToken}
        [] -> respond $ WithStatus @200 $ myNoHeader LoginResult {message = "Invalid username or password.", token = Nothing}
        _ -> respond $ WithStatus @500 LoginResult {message = "Unkown error occured.", token = Nothing}

loginApi :: Pool Connection -> Server LoginAPI
loginApi connectionPool = loginUser connectionPool
