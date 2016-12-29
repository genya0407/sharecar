{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Route.Reservation where

import           Web.Spock
import           Data.HVect

import qualified Model.Car as Car
import qualified Model.Reservation as Reservation
import qualified View as V
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
