module Linguine.Models.Course (getCourses) where

import Data.Aeson (ToJSON)
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, FromRow, query_)
import GHC.Generics (Generic)

data Course = Course
  { courseId :: Int,
    courseSourceLanguage :: String,
    courseTargetLanguage :: String,
    courseCreatedAt :: UTCTime,
    courseUri :: String
  }
  deriving (Generic, Show, FromRow, ToJSON)

getCourses :: Pool Connection -> IO ([Course])
getCourses pool =
  withResource pool $ \conn -> do
    query_ conn "SELECT id, source_language, target_language, created_at, uri from courses" :: IO [Course]
