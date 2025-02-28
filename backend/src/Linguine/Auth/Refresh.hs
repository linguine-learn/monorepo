{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Linguine.Auth.Refresh (RefreshAPI, refreshApi) where

import qualified Linguine.DB.Queries as DBQ

import Servant 

import GHC.Generics (Generic)

import Data.Pool (Pool, withResource)
import Data.Aeson (ToJSON)

import Database.PostgreSQL.Simple (Connection)
import Web.Cookie 
import qualified Data.ByteString.Char8 as BSC
import Linguine.Auth.JWT (parseRefresh, makeJwtPair)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import Linguine.DB.Models (User(user_refreshTokenVersion))
import Linguine.Auth.Login (makeRefreshCookie)

data RefreshResult = RefreshResult  {
  message :: String,
  token :: Maybe String
} deriving (Show, Generic, ToJSON)

setRefreshCookie :: String -> RefreshResult ->  Headers '[Header "Set-Cookie" String] RefreshResult
setRefreshCookie refreshToken loginResult = addHeader refreshToken loginResult

type RefreshAPI = "auth" :> "refresh" :> Header "Cookie" String :>
  UVerb 'POST  '[JSON] '[WithStatus 200 (Headers '[Header "Set-Cookie" String] RefreshResult),  WithStatus 403 RefreshResult]

refreshUser :: Pool Connection -> Maybe String -> Handler (Union '[WithStatus 200 (Headers '[Header "Set-Cookie" String] RefreshResult), WithStatus 403 RefreshResult])
refreshUser connectionPool maybeCookies = do
  case maybeCookies of
    Just cookies -> do
      let parsedCookies = parseCookiesText $ BSC.pack cookies
          maybeRefreshToken = (lookup "refreshToken" parsedCookies) >>= \refreshTokenByteString -> Just $ T.unpack refreshTokenByteString

      case maybeRefreshToken of
        Just oldRefreshToken -> do
          maybeParsedRefreshTokenData <- liftIO $ parseRefresh $ T.pack oldRefreshToken 
          case maybeParsedRefreshTokenData of
            Just (version, userId) -> do
              users <- liftIO $ withResource connectionPool $ \conn -> DBQ.getUserById conn userId
              case users of 
                [user] | (user_refreshTokenVersion user) == version -> do
                    (accessToken, newRefreshToken, refreshTokenExpiryTime) <- liftIO $ makeJwtPair (userId, user_refreshTokenVersion user)
                    let refreshCookie = makeRefreshCookie newRefreshToken refreshTokenExpiryTime
                    respond $ WithStatus @200 $ setRefreshCookie refreshCookie RefreshResult { message = "Success", token = Just accessToken }

                _ -> respond $ WithStatus @403 RefreshResult { message = "Invalid refresh token!", token = Nothing }
            Nothing -> respond $ WithStatus @403 RefreshResult { message = "Refresh token expired!", token = Nothing }
        Nothing -> respond $ WithStatus @403 RefreshResult { message = "No refresh token provided!", token = Nothing }
    Nothing -> respond $ WithStatus @403 RefreshResult { message = "No refresh token provided!", token = Nothing }

refreshApi :: Pool Connection -> Server RefreshAPI
refreshApi  connectionPool cookies = refreshUser connectionPool cookies
