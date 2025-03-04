module Linguine.Auth (setSessionCookie, setSessionCookieAndRedirect) where
import Linguine.Models.Session (Session(..))
import Web.Scotty (ActionM, setHeader, status)
import Web.Cookie
import Data.ByteString.UTF8 as BSU
import Linguine.Config (productionMode)
import Web.Scotty.Cookie (setCookie)
import Network.HTTP.Types (found302)
import Data.Text.Internal.Lazy (Text)

setSessionCookie :: Session -> ActionM()
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
