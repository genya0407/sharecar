{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Car where

import           Prelude hiding (all)
import           Database.Persist
import           Data.Text hiding (all)

import qualified Model.Occupation as Occup
import           Template

import           Utils

mkBoilerplate "Car"

new = Car "" defaultUTCTime defaultUTCTime

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
