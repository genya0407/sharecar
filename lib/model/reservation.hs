module Model.Reservation where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Utils

all :: MonadIO m => m [Entity Reservation]
all = runDB $ selectList [] []

active :: MonadIO m => m [Entity Reservation]
active = do
  now <- liftIO $ getCurrentTime'
  runDB $ selectList [ReservationEnd >=. now] []

type Begin = UTCTime
type End = UTCTime

create :: MonadIO m => UserId -> CarId -> Begin -> End -> m (Key Reservation)
create userid carid begin end = do
  now <- liftIO getCurrentTime'
  runDB . insert $ Reservation userid carid begin end now now
