module Linguine.Auth (setSessionCookie, setSessionCookieAndRedirect) where

import Data.ByteString.UTF8 as BSU
import Data.Text.Internal.Lazy (Text)
import Linguine.Config (productionMode)
import Linguine.Models.Session (Session (..))
import Web.Cookie
import Web.Scotty (ActionM, redirect)
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
  redirect redirectUrl
