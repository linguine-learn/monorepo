module Linguine.Auth.OAuth.Google (googleOAuthServer) where

import Configuration.Dotenv.Environment (lookupEnv)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.UTF8 as BSU
import Data.Either (fromRight)
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (secondsToDiffTime)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Auth.OAuth (generateState, parseOIDCToken)
import Linguine.Config (productionMode)
import Linguine.Models.Session (createSession, generateSessionToken, Session (sessionId, sessionExpiresAt))
import Linguine.Models.User (User (userId), getUserByEmail)
import Network.HTTP.Client (newManager)
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.HTTP.Types (badRequest400, found302)
import Network.OAuth.OAuth2 (ExchangeToken (ExchangeToken), IdToken (idtoken), OAuth2 (..), OAuth2Token (idToken), appendQueryParams, authorizationUrl, fetchAccessToken)
import Text.Printf (printf)
import URI.ByteString (URI, parseURI, serializeURIRef', strictURIParserOptions)
import Web.Cookie
import Web.Scotty (ActionM, ScottyM, get, queryParamMaybe, setHeader, status, text)
import Web.Scotty.Cookie (getCookie, setCookie)
import System.Environment (getEnv)

uriToText :: URI -> TL.Text
uriToText = TL.fromStrict . T.decodeUtf8 . serializeURIRef'

google :: IO (Maybe OAuth2)
google = do
  maybeGoogleClientId <- lookupEnv "GOOGLE_WEB_CLIENT_ID"
  maybeGoogleClientSecret <- lookupEnv "GOOGLE_WEB_CLIENT_SECRET"
  maybeBaseUrl <- lookupEnv "BASE_URL"

  case (maybeGoogleClientId, maybeGoogleClientSecret, maybeBaseUrl) of
    (Just googleClientId, Just googleClientSecret, Just baseUrl) -> do
      let redirectUriString :: String = printf "%s/%s" baseUrl ("login/google/callback" :: String)
          redirectUri = fromRight (error "Failed to parse redirectUri") $ parseURI strictURIParserOptions $ BSU.fromString redirectUriString
          tokenEndpoint = fromRight (error "Failed to parse google token endpoint") $ parseURI strictURIParserOptions "https://oauth2.googleapis.com/token"
          authorizeEndpoint = fromRight (error "Failed to parse google authorize endpoint") $ parseURI strictURIParserOptions "https://accounts.google.com/o/oauth2/v2/auth"
      pure $
        Just
          OAuth2
            { oauth2ClientId = T.pack googleClientId,
              oauth2ClientSecret = T.pack googleClientSecret,
              oauth2AuthorizeEndpoint = authorizeEndpoint,
              oauth2TokenEndpoint = tokenEndpoint,
              oauth2RedirectUri = redirectUri
            }
    _ -> pure $ Nothing

authorizationUrlHandler :: ActionM ()
authorizationUrlHandler = do
  maybeOauthGoogle <- liftIO $ google
  case maybeOauthGoogle of
    Just oauthGoogle -> do
      state <- liftIO $ generateState
      let authorizeUrl = appendQueryParams [("scope", "email"), ("state", BSU.fromString state)] $ authorizationUrl oauthGoogle
          tenMinutes = 60 * 10
          oauthStateCookie =
            defaultSetCookie
              { setCookieName = "google_oauth_state",
                setCookieValue = BSU.fromString state,
                setCookiePath = Just "/",
                setCookieHttpOnly = True,
                setCookieSecure = productionMode,
                setCookieMaxAge = Just $ secondsToDiffTime tenMinutes,
                setCookieSameSite = Just sameSiteLax
              }
      setCookie oauthStateCookie
      setHeader "Location" (uriToText authorizeUrl)
      status found302
    _ -> text "Google OAuth is not supported at this time."

validationCallbackHandler :: Pool Connection -> ActionM ()
validationCallbackHandler pool = do
  maybeCode :: Maybe String <- queryParamMaybe "code"
  maybeState :: Maybe String <- queryParamMaybe "state"
  maybeStoredState :: Maybe T.Text <- getCookie "google_oauth_state"
  maybeOauthGoogle <- liftIO $ google
  case (maybeCode, maybeState, maybeStoredState, maybeOauthGoogle) of
    (Just code, Just state, Just storedState, Just oauthGoogle) | code /= "" && state == T.unpack storedState -> do
      manager <- liftIO $ newManager tlsManagerSettings
      let codeToken = ExchangeToken (T.pack code)
      tokenResponse <- runExceptT $ fetchAccessToken manager oauthGoogle $ codeToken
      case tokenResponse of
        Right token -> do
          let maybeVerifiedEmail = (idToken token) >>= \idToken -> parseOIDCToken $ idtoken idToken
          case maybeVerifiedEmail of
            Just verifiedEmail -> do
              existingUser <- liftIO $ getUserByEmail pool verifiedEmail
              case existingUser of
                Just user -> do
                  sessionToken <- liftIO generateSessionToken
                  session <- liftIO $ createSession pool sessionToken (userId user)
                  let sessionCookie = defaultSetCookie
                        { setCookieName = "session",
                          setCookiePath = Just "/",
                          setCookieValue = BSU.fromString $ sessionId session,
                          setCookieHttpOnly = True,
                          setCookieSecure = productionMode,
                          setCookieExpires = Just $ sessionExpiresAt session,
                          setCookieSameSite = Just sameSiteLax
                        }
                  setCookie sessionCookie
                  liftIO $ print sessionCookie
                  status found302
                  redirectUrl <- liftIO $ getEnv "OAUTH_REDIRECT_URL"
                  setHeader "Location" (TL.fromStrict $ T.pack redirectUrl)
                Nothing -> undefined
            _ -> status badRequest400
        _ -> status badRequest400
    _ -> status badRequest400

googleOAuthServer :: Pool Connection -> ScottyM ()
googleOAuthServer pool = do
  get "/login/google" authorizationUrlHandler
  get "/login/google/callback" $ validationCallbackHandler pool
