{-# LANGUAGE TemplateHaskell #-}

module Model.Session where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Utils
import           Template

mkBoilerplate "Session"

new = Session (toSqlKey 0) defaultUTCTime defaultUTCTime

validUntil :: Session -> UTCTime
validUntil = addUTCTime (60 * 24 * 14) . sessionUpdated
