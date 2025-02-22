{-# LANGUAGE OverloadedStrings #-}
module Linguine.Auth.JWT (makeJwtPair) where

import qualified Web.JWT as J
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Aeson (Value(Number))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time.Clock.POSIX (getPOSIXTime, posixDayLength)
import System.Environment (getEnv)
import Web.JWT (hmacSecret)

makeJwtPair :: (Int, Int) -> IO (String, String)
makeJwtPair (userId, refreshTokenVersion) = do
  currentTime <- liftIO getPOSIXTime

  let fiveteenMinutes = 60 * 15
  let accessTokenExpireTime = currentTime + fiveteenMinutes
  let refreshTokenExpireTime = currentTime + posixDayLength * 30

  let accessTokenData = mempty {
    J.iss = J.stringOrURI "mybox-backend",
      J.iat = J.numericDate currentTime,
      J.exp = J.numericDate accessTokenExpireTime,
      J.unregisteredClaims = J.ClaimsMap $ Map.fromList [
        ("userId", Number $ fromIntegral userId)
      ]
  }

  accessSecret <- liftIO $ getEnv "ACCESS_SECRET"
  let accessKey = hmacSecret . T.pack $ accessSecret
  let accessToken = J.encodeSigned accessKey mempty accessTokenData

  let refreshTokenData = mempty {
    J.iss = J.stringOrURI "mybox-backend",
      J.iat = J.numericDate currentTime,
      J.exp = J.numericDate refreshTokenExpireTime,
      J.unregisteredClaims = J.ClaimsMap $ Map.fromList [
        ("userId", Number $ fromIntegral userId),
        ("version", Number $ fromIntegral refreshTokenVersion)
      ]
  }

  refreshSecret <- liftIO $ getEnv "REFRESH_SECRET"
  let refreshKey = hmacSecret . T.pack $ refreshSecret
  let refreshToken = J.encodeSigned refreshKey mempty refreshTokenData 

  pure (T.unpack accessToken, T.unpack refreshToken)

