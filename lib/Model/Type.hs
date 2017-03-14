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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model.Type where

import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Format
import           Data.Time.Calendar
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           GHC.Generics
import           Control.Monad.IO.Class

import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource.Internal

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Lucid.Base

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    mail Text
    name Text
    cryptPassword ByteString
    phoneNumber Text
    updated UTCTime
    created UTCTime
    UniqueMail mail
    deriving Show Generic
  Car
    name Text
    updated UTCTime
    created UTCTime
    deriving Show Generic
  Reservation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    updated UTCTime
    created UTCTime
    deriving Show Generic Eq
  Occupation
    userId UserId
    carId CarId
    begin UTCTime
    end UTCTime
    meterBegin Int
    meterEnd Int Maybe
    updated UTCTime
    created UTCTime
    deriving Show Generic
  Session
    userId UserId
    updated UTCTime
    created UTCTime
    deriving Show Generic
  Gas
    userId UserId
    carId CarId
    amount Int
    updated UTCTime
    created UTCTime
    deriving Show Generic
|]

build :: Monad m => Builder -> HtmlT m ()
build b = HtmlT (return (const b,()))

instance (ToBackendKey SqlBackend record) => ToHtml (Key record) where
  toHtml    = build . Blaze.fromString . show . fromSqlKey
  toHtmlRaw = build . Blaze.fromString . show . fromSqlKey

instance ToHtml UTCTime where
  toHtml    = build . Blaze.fromString . (formatTime defaultTimeLocale "%Y年%m月%d日 %H時%M分")
  toHtmlRaw = toHtml

instance ToHtml Day where
  toHtml    = build . Blaze.fromString . (formatTime defaultTimeLocale "%Y年%m月%d日")
  toHtmlRaw = toHtml

instance Ord Reservation where
  res1 `compare` res2 = (reservationBegin res1) `compare` (reservationBegin res2)

_runDB :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
_runDB query = runSqlite "db/db.sqlite3" query

runDB :: MonadIO m => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
runDB = liftIO . _runDB
