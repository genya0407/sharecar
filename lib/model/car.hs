module Model.Car where

import           Prelude hiding (all)
import           Model.Type
import qualified Model.Occupation as Occup
import           Database.Persist
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text hiding (all)

all :: MonadIO m => m [Entity Car]
all = runDB $ selectList [] []

create :: MonadIO m => Text -> m (Key Car)
create name = runDB . insert $ Car name

allWithOccupied :: MonadIO m => m [(Entity Car, Bool)]
allWithOccupied = do
  cars <- all
  carsWithOccupied <- forM cars $ \(Entity carid car) -> do
    isOccupied <- Occup.isOccupied carid
    return (Entity carid car, isOccupied)
  return carsWithOccupied

withOccupied :: MonadIO m => CarId -> m (Maybe (Entity Car, Bool))
withOccupied carid = do
  Just car <- runDB $ get carid
  isOccupied <- Occup.isOccupied carid
  return $ Just (Entity carid car, isOccupied)
