module Linguine.Models.Session (Session (..), SessionValidationResult (..), ValidSession (..), createSession, generateSessionToken, validateSessionToken, invalidateSession, invalidateSessions) where

import Control.Monad (void)
import Data.Pool (Pool, withResource)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixDayLength)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, Only (Only), execute, query)
import GHC.Generics (Generic)
import Linguine.Models.User qualified as MUser

data Session = Session
  { sessionId :: String,
    sessionUserId :: Int,
    sessionExpiresAt :: UTCTime
  }
  deriving (Generic, Show)

generateSessionToken :: IO String
generateSessionToken = do
  uuid <- nextRandom
  pure $ toString uuid

createSession :: Pool Connection -> String -> Int -> IO Session
createSession pool sessionToken userId = do
  currentTime <- getCurrentTime
  let expiresAt = addUTCTime (30 * nominalDay) currentTime
      session = Session {sessionId = sessionToken, sessionUserId = userId, sessionExpiresAt = expiresAt}
  withResource pool $ \conn -> do
    void $ execute conn "INSERT INTO user_session (id, user_id, expires_at) VALUES (?, ?, ?)" (sessionToken, userId, expiresAt)
    pure ()
  pure session

data ValidSession = ValidSession {session :: Session, user :: MUser.User} deriving (Generic, Show)

data SessionValidationResult
  = Valid ValidSession
  | Invalid

instance Show SessionValidationResult where
  show (Valid validSession) = show validSession
  show Invalid = "Invalid"

invalidateSession :: Pool Connection -> String -> IO ()
invalidateSession pool sessionId = do
  void $ withResource pool $ \conn -> do
    execute conn "DELETE FROM user_session WHERE id = ?" (Only sessionId)
  pure ()

increaseSessionDuration :: Pool Connection -> String -> NominalDiffTime -> IO UTCTime
increaseSessionDuration pool sessionId deltaTime = do
  currentTime <- getCurrentTime
  let newTime = addUTCTime deltaTime currentTime
  void $ withResource pool $ \conn -> do
    execute conn "UPDATE user_session SET expires_at = ? WHERE id = ?" (sessionId, newTime)
  pure newTime

invalidateSessions :: Pool Connection -> String -> IO ()
invalidateSessions pool userId = do
  void $ withResource pool $ \conn -> do
    execute conn "DELETE FROM user_session WHERE user_id = ?" (Only userId)
  pure ()

validateSessionToken :: Pool Connection -> String -> IO SessionValidationResult
validateSessionToken pool sessionId = do
  withResource pool $ \conn -> do
    rows <- query conn "SELECT user_session.id, user_session.user_id, user_session.expires_at, users.email, users.password, users.created_at FROM user_session INNER JOIN users ON users.id = user_session.user_id WHERE  user_session.id = ?" (Only sessionId) :: IO [(String, Int, UTCTime, String, Maybe String, UTCTime)]
    case rows of
      [row] -> do
        let (rSessionId, rUserId, rSessionExpiresAt, userEmail, userPassword, userCreatedAt) = row
            userSession = Session {sessionId = rSessionId, sessionUserId = rUserId, sessionExpiresAt = rSessionExpiresAt}
            user = MUser.User {MUser.userId = rUserId, MUser.email = userEmail, MUser.password = userPassword, MUser.createdAt = userCreatedAt}
        currentTime <- getCurrentTime
        case currentTime >= rSessionExpiresAt of
          True -> do
            invalidateSession pool rSessionId
            pure Invalid
          False -> do
            let sessionExpiresAfterFifteenDays = currentTime >= (addUTCTime (-15 * posixDayLength) rSessionExpiresAt)
            case sessionExpiresAfterFifteenDays of
              True -> do
                newExpiryTime <- increaseSessionDuration pool rSessionId (30 * posixDayLength)
                let renewedSession = Session {sessionId = rSessionId, sessionUserId = rUserId, sessionExpiresAt = newExpiryTime}
                pure $ Valid ValidSession {session = renewedSession, user = user}
              False -> pure $ Valid ValidSession {session = userSession, user = user}
      _ -> pure Invalid
