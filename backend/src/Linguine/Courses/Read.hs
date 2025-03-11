module Linguine.Courses.Read (getAllCoursesHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Models.Course (getCourses)
import Web.Scotty (ActionM, json)

getAllCoursesHandler :: Pool Connection -> ActionM ()
getAllCoursesHandler pool = do
  courses <- liftIO $ getCourses pool
  json courses
