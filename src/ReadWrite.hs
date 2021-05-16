{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module ReadWrite where

import           Model as D
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.Text                   hiding (head, map)
import           Data.Time
import           Data.Int(Int64)
import           Database.Esqueleto
import           Database.Persist.Postgresql (createPostgresqlPool,
                                              withPostgresqlConn, ConnectionString)
import           ClassyPrelude


connectionString :: ConnectionString  
connectionString = "host=localhost port=5432 user=kyrdev dbname=ishber password=s3cr3t"

runDB :: (MonadUnliftIO m, MonadIO m, MonadBaseControl IO m)
      => ReaderT SqlBackend (LoggingT m) a 
      -> m a
runDB q = runStderrLoggingT $ withPostgresqlConn connectionString $ \b -> runReaderT q b

doingMigration :: (MonadUnliftIO m, MonadIO m, MonadBaseControl IO m, MonadLogger m)
               => m ()
doingMigration = do
  pool <- createPostgresqlPool connectionString 5
  liftIO $ runSqlPool doMigration  pool

insertUser :: (MonadUnliftIO m, MonadIO m, MonadBaseControl IO m) => User -> m Int64
insertUser u = do
  keyUser <- runDB $ insert u
  return (fromSqlKey keyUser)

getUser :: (MonadUnliftIO m, MonadIO m, MonadBaseControl IO m) 
        => Int64
	-> m (Maybe User)
getUser uId = runDB $ get (toSqlKey uId)
   
