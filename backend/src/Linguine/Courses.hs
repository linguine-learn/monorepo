module Linguine.Courses (coursesServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Courses.Read (getAllCoursesHandler)
import Web.Scotty (ScottyM, get)

coursesServer :: Pool Connection -> ScottyM ()
coursesServer pool = do
  get "/auth/courses" $ getAllCoursesHandler pool
