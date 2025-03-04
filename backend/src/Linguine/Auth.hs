module Linguine.Auth (setSessionCookie, setSessionCookieAndRedirect) where

import Data.ByteString.UTF8 as BSU
import Data.Text.Internal.Lazy (Text)
import Linguine.Config (productionMode)
import Linguine.Models.Session (Session (..))
import Network.HTTP.Types (found302)
import Web.Cookie
import Web.Scotty (ActionM, setHeader, status)
import Web.Scotty.Cookie (setCookie)

setSessionCookie :: Session -> ActionM ()
setSessionCookie session = do
  let sessionCookie =
        defaultSetCookie
          { setCookieName = "session",
            setCookiePath = Just "/",
            setCookieValue = BSU.fromString $ sessionId session,
            setCookieHttpOnly = True,
            setCookieSecure = productionMode,
            setCookieExpires = Just $ sessionExpiresAt session,
            setCookieSameSite = Just sameSiteLax
          }
  setCookie sessionCookie

setSessionCookieAndRedirect :: Session -> Text -> ActionM ()
setSessionCookieAndRedirect session redirectUrl = do
  setSessionCookie session
  status found302
  setHeader "Location" redirectUrl
