{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Aeson
import GHC.Generics

share 
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User 
    name Text 
    email Text 
    age Int 
    occupation Text
    UniqueEmail email
    deriving Show Read Generic
  |]

doMigration :: SqlPersistT IO()
doMigration = runMigration migrateAll

