{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Model where

import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(..))
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sqlite
import           GHC.Generics
import           Data.Aeson
import           Test.QuickCheck
import           Data.Text.Arbitrary
import           Control.Monad.IO.Class

import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    mail Text
    name Text
    phoneNumber Text
    UniqueMail mail
    deriving Show Generic
  Car
    name Text
    deriving Show Generic
  Reservation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    updated UTCTime
    created UTCTime
    deriving Show Generic
  Occupation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    meterBegin UTCTime
    meterEnd UTCTime
    updated UTCTime
    created UTCTime
    deriving Show Generic
|]

instance FromJSON User
instance FromJSON Car
instance FromJSON Reservation
instance FromJSON Occupation

instance ToJSON User
instance ToJSON Car
instance ToJSON Reservation
instance ToJSON Occupation

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary

runDB :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB query = runSqlite "db/db.sqlite3" query
