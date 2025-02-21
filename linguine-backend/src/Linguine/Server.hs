module Linguine.Server where

import Linguine.Auth.Register (RegisterAPI, registerApi)
import Servant.Server
import Data.Proxy
import Network.Wai.Handler.Warp (run)

type LinguineAPI = RegisterAPI

linguineApi :: Server LinguineAPI
linguineApi = registerApi

app :: Application
app = serve (Proxy :: Proxy LinguineAPI) linguineApi

serveLinguine :: IO ()
serveLinguine = do
  putStrLn "Running on port 3000"
  run 3000 app
