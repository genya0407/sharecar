module Model.Occupation where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Time.Clock

all :: MonadIO m => m [Entity Occupation]
all = runDB $ selectList [] []

isOccupied :: MonadIO m => CarId -> m Bool
isOccupied carid = do
  now <-  liftIO getCurrentTime
  mOccupation <- runDB $ selectFirst [OccupationCarId ==. carid, OccupationEnd >. now] []
  case mOccupation of
    Just _ -> return True
    Nothing -> return False
