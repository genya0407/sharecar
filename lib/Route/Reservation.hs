{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Route.Reservation where

import           Web.Spock
import           Data.HVect

import qualified Model.Car as Car
import qualified Model.Reservation as Reservation
import qualified View as V
import qualified Form as F
import           Route.Url

import           Utils

reservationRoute :: SpockCtxM (HVect (Entity User ': '[])) () SessionVal MyAppState ()
reservationRoute = do
  get ("/car" <//> var <//> "reservation") $ \_carid -> do
    let carid = toSqlKey _carid
    (me :: Entity User) <- liftM findFirst getContext
    mCwo <- Car.withOccupied carid
    rwu <- Reservation.activeReservationsWithUser carid
    case mCwo of
      Just cwo -> html $ V.reservations_ me cwo rwu
      _ -> redirect $ carDetailUrl carid
  get ("/car" <//> var <//> "reservation/new") $ \_carid -> do
    let carid = toSqlKey _carid
    (me :: Entity User) <- liftM findFirst getContext
    html $ V.newReservation_ me carid
  post ("/car" <//> var <//> "reservation/new") $ \_carid -> do
    let carid = toSqlKey _carid
    (Entity meid me :: Entity User) <- liftM findFirst getContext
    mReserv <- F.formReservation meid carid
    case mReserv of
      Just res -> do
        Reservation.create res
        redirect $ reservationsUrl carid
      Nothing -> redirect $ newReservationUrl carid
  get ("/reservation" <//> var <//> "edit") $ \_resid -> do
    let resid = toSqlKey _resid
    (me :: Entity User) <- liftM findFirst getContext
    mRes <- Reservation.find resid
    case mRes of
      Just res -> do
        if reservationUserId res == entityKey me then
          html $ V.editReservation_ me (Entity resid res)
        else
          redirect $ reservationsUrl (reservationCarId res)
      Nothing -> redirect "/car"
  post ("/reservation" <//> var <//> "edit") $ \_resid -> do
    let resid = toSqlKey _resid
    (me :: Entity User) <- liftM findFirst getContext
    mCarid <- Reservation.getCarId resid
    case mCarid of
      Just carid -> do
        mRes <- F.formReservation (entityKey me) carid
        case mRes of
          Just res -> do
            if reservationUserId res == entityKey me then do
              Reservation.update resid res
              redirect $ reservationsUrl (reservationCarId res)
            else
              redirect "/car"
          Nothing -> redirect "/car"
      Nothing -> redirect "/car"
