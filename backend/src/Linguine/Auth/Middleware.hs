module Linguine.Auth.Middleware (authMiddleware) where

import Data.ByteString.UTF8 as BSU
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Models.Session (SessionValidationResult (InvalidSession), validateSessionToken)
import Network.HTTP.Types (unauthorized401)
import Network.Wai (Middleware, Request (requestHeaders), responseLBS)
import Web.Cookie (Cookies, parseCookies)

authMiddleware :: Pool Connection -> Middleware
authMiddleware pool app request respond = do
  let headers = requestHeaders request
      maybeCookies = lookup "cookie" headers
      parsedCookies :: Maybe Cookies = maybeCookies >>= Just . parseCookies
      maybeSessionId :: Maybe ByteString = parsedCookies >>= lookup "session"
  case maybeSessionId of
    Just sessionId -> do
      result <- validateSessionToken pool $ BSU.toString sessionId
      case result of
        InvalidSession -> respond $ responseLBS unauthorized401 [] ""
        _ -> app request respond
    _ -> respond $ responseLBS unauthorized401 [] ""
