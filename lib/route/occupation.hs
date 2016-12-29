{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Route.Occupation where

import           Web.Spock
import           Database.Persist hiding (get)
import qualified Data.Text as T
import           Data.HVect

import qualified View as V
import qualified Model.Occupation as Occup
import qualified Model.Car as Car
import qualified Form as F

import           Utils

occupationRoute :: SpockCtxM (HVect (Entity User ': '[])) () SessionVal MyAppState ()
occupationRoute = do
  get ("car" <//> var <//> "occupy/new") $ \_carid -> do
    let carid = toSqlKey _carid
    (me :: Entity User) <- liftM findFirst getContext
    mCarWithOccupied <- Car.withOccupied carid
    case mCarWithOccupied of
      Just carWithOccupied@(Entity carid car, isOccupied) -> do
        now <- liftIO getCurrentTime'
        mLastOccup <- Occup.lastByMeter carid
        html $ V.occupyNew_ me carWithOccupied (Just now) (occupationMeterEnd =<< (\(Entity occupid occup) -> Just occup) =<< mLastOccup)
      Nothing -> redirect "/car"
  post ("car" <//> var <//> "occupy/new") $ \_carid -> do
    let carid = toSqlKey _carid
    Entity meid me <- liftM findFirst getContext
    mOccup <- F.formOccupationBegin meid carid
    case mOccup of
      Just occup -> do
        Occup.create occup
        redirect "/car"
      Nothing -> do
        redirect . T.pack $ "/car/" ++ show _carid ++ "/occupy/new"
  get ("occupation" <//> var <//> "edit") $ \_occupid -> do
    let occupid = toSqlKey _occupid
    (meEntity@(Entity meid me) :: Entity User) <- liftM findFirst getContext
    mOccup <- Occup.find occupid
    case mOccup of
      Just occup -> do
        if occupationUserId occup == meid then do
          mWithOccupied <- Car.withOccupied $ occupationCarId occup
          case mWithOccupied of
            Just withOccupied -> html $ V.occupationEdit_ meEntity withOccupied (Entity occupid occup)
            Nothing -> redirect "/car"
        else
          redirect "/car"
      Nothing -> redirect "/car"
  post ("occupation" <//> var <//> "edit") $ \_occupid -> do
    let occupid = toSqlKey _occupid
    (meEntity@(Entity meid me) :: Entity User) <- liftM findFirst getContext
    mOccup <- Occup.find occupid
    case mOccup of
      Just occup -> do
        if occupationUserId occup == meid then do
          mMeterEnd <- param "meter-end"
          case mMeterEnd of
            Just meterEnd -> do
              Occup.replace occupid $ occup { occupationMeterEnd = Just meterEnd }
              redirect "/car"
            Nothing -> redirect "/car"
        else
          redirect "/car"
      Nothing -> redirect "/car"
