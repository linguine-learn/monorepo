module Linguine.Courses.Read (getAllCoursesHandler, getSingleCourseHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Models.Course (getCourseById, getCourses)
import Web.Scotty (ActionM, json, pathParam)

getAllCoursesHandler :: Pool Connection -> ActionM ()
getAllCoursesHandler pool = do
  courses <- liftIO $ getCourses pool
  json courses

-- TODO: look into returning course JSON instead of metadata
getSingleCourseHandler :: Pool Connection -> ActionM ()
getSingleCourseHandler pool = do
  courseId :: Int <- pathParam "courseId"
  maybeCourse <- liftIO $ getCourseById pool courseId
  json maybeCourse
