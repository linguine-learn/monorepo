{-# LANGUAGE OverloadedStrings #-}
module Linguine.Auth.JWT (makeJwtPair, verifyUser) where

import qualified Web.JWT as J
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Aeson (Value(Number), FromJSON (parseJSON))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time.Clock.POSIX (getPOSIXTime, posixDayLength, posixSecondsToUTCTime)
import System.Environment (getEnv)
import Web.JWT (hmacSecret)
import Data.Time (UTCTime)
import Data.Aeson.Types (parseMaybe)

makeJwtPair :: (Int, Int) -> IO (String, String, UTCTime)
makeJwtPair (userId, refreshTokenVersion) = do
  currentTime <- liftIO getPOSIXTime

  let fiveteenMinutes = 60 * 15
      accessTokenExpireTime = currentTime + fiveteenMinutes
      refreshTokenExpireTime = currentTime + posixDayLength * 30
      accessTokenData = mempty {
        J.iss = J.stringOrURI "mybox-backend",
          J.iat = J.numericDate currentTime,
          J.exp = J.numericDate accessTokenExpireTime,
          J.unregisteredClaims = J.ClaimsMap $ Map.fromList [
            ("userId", Number $ fromIntegral userId)
          ]
      }

  accessSecret <- liftIO $ getEnv "ACCESS_SECRET"
  let accessKey = hmacSecret . T.pack $ accessSecret
      accessToken = J.encodeSigned accessKey mempty accessTokenData
      refreshTokenData = mempty {
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
      refreshToken = J.encodeSigned refreshKey mempty refreshTokenData 

  pure (T.unpack accessToken, T.unpack refreshToken, posixSecondsToUTCTime refreshTokenExpireTime)

verifyUser :: T.Text -> IO (Maybe Int)
verifyUser accessToken  = do
  currentTime <- getPOSIXTime

  accessSecret <- liftIO $ getEnv "ACCESS_SECRET"
  let accessKey = hmacSecret . T.pack $ accessSecret
      maybeJwt = J.decodeAndVerifySignature (J.toVerify accessKey) accessToken

  case maybeJwt of
    Just accessJwt -> do 
      let maybeExpiry = J.exp $ J.claims accessJwt

      pure $ maybeExpiry >>= \expiryTime -> do
          if (J.secondsSinceEpoch expiryTime) <= currentTime then do
            let accessClaims = J.unClaimsMap $ J.unregisteredClaims $ J.claims accessJwt
                maybeUserIdValue = Map.lookup "userId" accessClaims
            maybeUserIdValue >>= \userIdValue -> parseMaybe parseJSON userIdValue
          else Nothing
    Nothing -> pure Nothing
