{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString, fromSqlKey)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Model
import           ReadWrite(getUser, insertUser, doingMigration)
import           Api
import           Data.Aeson
import           GHC.Generics
import           Control.Monad.Logger

fetchUsersHandler :: Int64 -> Handler User
fetchUsersHandler userId = do
  maybeUser <- liftIO $ getUser userId
  case maybeUser of
    Just user ->  return user
    Nothing   ->  Handler (throwE $ err401 { errBody = "Could not find the user"})

insertUsersHandler :: User -> Handler Int64 
insertUsersHandler user = liftIO $ insertUser user

instance ToJSON User where
instance FromJSON User where

usersServer :: Server Api
usersServer = fetchUsersHandler :<|> insertUsersHandler

runServer :: IO ()
runServer = do
  runStderrLoggingT doingMigration
  run 8000 (serve api usersServer)
