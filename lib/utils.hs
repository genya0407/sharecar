module Utils (
  module Utils,
  module Data.Time.Clock,
  module Data.Time.Calendar,
  module Data.Time.Format,
  module Control.Monad.IO.Class,
  module Control.Monad,
  module Control.Monad.Trans,
  module Model.Type,
  toSqlKey, fromSqlKey,
  Entity(..)
) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.IORef
import Control.Monad.Trans
import Control.Monad
import Control.Monad.IO.Class
import Database.Persist.Sql

import Model.Type
import Database.Persist (Entity(..))

type SessionVal = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)

getCurrentTime' :: MonadIO m => m UTCTime
getCurrentTime' = do
  now <- liftIO getCurrentTime
  return $ addUTCTime (60 * 60 * 9) now

defaultUTCTime = UTCTime (ModifiedJulianDay 0) 0
