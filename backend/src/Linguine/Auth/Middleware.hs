module Linguine.Auth.Middleware (authMiddleware) where

import Data.ByteString.UTF8 as BSU
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Models.Session (SessionValidationResult (Invalid, Valid), validateSessionToken, ValidSession)
import Network.HTTP.Types (unauthorized401)
import Network.Wai (Middleware, Request (requestHeaders, vault), responseLBS)
import Web.Cookie (Cookies, parseCookies)
import qualified Data.Vault.Lazy as V

authMiddleware :: V.Key ValidSession -> Pool Connection -> Middleware
authMiddleware key pool app request respond = do
  let headers = requestHeaders request
      maybeCookies = lookup "cookie" headers
      parsedCookies :: Maybe Cookies = maybeCookies >>= Just . parseCookies
      maybeSessionId :: Maybe ByteString = parsedCookies >>= lookup "session"
  case maybeSessionId of
    Just sessionId -> do
      sessionResult <- validateSessionToken pool $ BSU.toString sessionId
      case sessionResult  of
        Invalid -> respond $ responseLBS unauthorized401 [] ""
        (Valid result) -> do
          let vault' = V.insert key result (vault request)
              request' = request {vault = vault'}
          app request' respond
    _ -> respond $ responseLBS unauthorized401 [] ""
