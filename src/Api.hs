{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Servant.API
import Data.Int(Int64)
import Model

type Api =
     "users" :> Capture "userid" Int64 :> Get '[JSON] User 
     :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64 
    

api :: Proxy Api
api = Proxy :: Proxy Api
