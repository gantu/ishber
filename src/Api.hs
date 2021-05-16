module Api where

import Data.Proxy
import Servant.API
import Data.Int(Int64)
import Model
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

type ProtectedApi =
     "users" :> Capture "userid" Int64 :> Get '[JSON] User 
     :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64 
    
type UnprotectedApi = "api"
  :> "users"
  :> "login" 
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie]
					NoContent)
  :<|> Raw 

type API auths = (Servant.Auth.Server.Auth auths User :> ProtectedApi) :<|> UnprotectedApi

api :: Proxy ProtectedApi
api = Proxy :: Proxy ProtectedApi
