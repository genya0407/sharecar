module Model.Occupation where

import           Model.Type
import           Database.Persist as P
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Utils

new :: MonadIO m => UserId -> CarId -> UTCTime -> UTCTime -> Int -> Maybe Int -> m Occupation
new userid carid begin end meterBegin mMeterEnd = do
  now <- liftIO getCurrentTime'
  return $ Occupation userid carid begin end meterBegin mMeterEnd now now

save :: MonadIO m => Occupation -> m OccupationId
save = runDB . insert

find :: MonadIO m => OccupationId -> m (Maybe Occupation)
find occupid = runDB $ get occupid

all :: MonadIO m => m [Entity Occupation]
all = runDB $ selectList [] []

replace :: MonadIO m => OccupationId -> Occupation -> m ()
replace occupid occup = runDB $ P.replace occupid occup

notMeterEndBy :: MonadIO m => UserId -> m [Entity Occupation]
notMeterEndBy userid = do
  now <- liftIO getCurrentTime'
  runDB $ selectList [OccupationUserId ==. userid, OccupationMeterEnd ==. Nothing] []

lastByMeter :: MonadIO m => CarId -> m (Maybe (Entity Occupation))
lastByMeter carid = runDB $ selectFirst [OccupationCarId ==. carid] [Desc OccupationMeterBegin]

isOccupied :: MonadIO m => CarId -> m Bool
isOccupied carid = do
  now <-  liftIO getCurrentTime'
  mOccupation <- runDB $ selectFirst [OccupationCarId ==. carid, OccupationEnd >. now] []
  case mOccupation of
    Just _ -> return True
    Nothing -> return False
