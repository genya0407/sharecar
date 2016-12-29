{-# LANGUAGE TemplateHaskell #-}
module Model.Occupation where

import           Model.Type
import           Database.Persist as P
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Data.Maybe
import           Utils
import           Template

mkBoilerplate "Occupation"

new = Occupation (toSqlKey 0) (toSqlKey 0) defaultUTCTime defaultUTCTime 0 Nothing defaultUTCTime defaultUTCTime

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
  mOccupation <- runDB $ selectFirst [OccupationCarId ==. carid, OccupationMeterEnd ==. Nothing] []
  return $ isJust mOccupation
