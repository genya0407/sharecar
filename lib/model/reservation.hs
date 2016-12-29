{-# LANGUAGE TemplateHaskell #-}
module Model.Reservation where

import           Database.Persist
import           Template
import           Utils

mkBoilerplate "Reservation"

new = Reservation (toSqlKey 0) (toSqlKey 0) defaultUTCTime defaultUTCTime defaultUTCTime defaultUTCTime

active :: MonadIO m => m [Entity Reservation]
active = do
  now <- liftIO $ getCurrentTime'
  runDB $ selectList [ReservationEnd >=. now] []

type Begin = UTCTime
type End = UTCTime

createTs :: MonadIO m => UserId -> CarId -> Begin -> End -> m (Key Reservation)
createTs userid carid begin end = do
  now <- liftIO getCurrentTime'
  create $ Reservation userid carid begin end now now
