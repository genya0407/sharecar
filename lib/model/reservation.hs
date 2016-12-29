{-# LANGUAGE TemplateHaskell #-}
module Model.Reservation where

import           Database.Persist

import qualified Model.User as User

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

activeReservationsWithUser :: MonadIO m => CarId -> m [(Entity Reservation, Entity User)]
activeReservationsWithUser carid = do
  now <- getCurrentTime'
  reservations <- runDB $ selectList [ReservationEnd >=. now, ReservationCarId ==. carid] []
  forM reservations $ \reservation -> do
    let userid = reservationUserId . entityVal $ reservation
    Just user <- User.find userid
    return (reservation, Entity userid user)
