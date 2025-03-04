module Linguine.Auth.EmailPassword (emailPasswordAuthServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Linguine.Auth.EmailPassword.Login (emailPasswordLoginHandler)
import Linguine.Auth.EmailPassword.Register (emailPasswordRegisterHandler)
import Web.Scotty (ScottyM, post)

emailPasswordAuthServer :: Pool Connection -> ScottyM ()
emailPasswordAuthServer pool = do
  post "/register" $ emailPasswordRegisterHandler pool
  post "/login" $ emailPasswordLoginHandler pool
