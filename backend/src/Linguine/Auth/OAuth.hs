module Linguine.Auth.OAuth (generateState, parseOIDCToken) where

import Data.Aeson (Value (Bool, String))
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Web.JWT qualified as J

generateState :: IO String
generateState = do
  uuid <- nextRandom
  pure $ toString uuid

parseOIDCToken :: Text -> Maybe String
parseOIDCToken trustedToken = do
  let maybeJwt = J.decode trustedToken
  case maybeJwt of
    Just jwt -> do
      let claimsMap = J.unClaimsMap $ J.unregisteredClaims $ J.claims jwt
          maybeEmail = Map.lookup "email" claimsMap
          maybeEmailVerified = Map.lookup "email_verified" claimsMap
      case (maybeEmail, maybeEmailVerified) of
        (Just (String email), Just (Bool emailVerified)) | emailVerified -> Just $ unpack email
        _ -> Nothing
    _ -> Nothing
