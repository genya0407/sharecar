module Route.Url where

import Data.Monoid ((<>))
import Data.Text

import Utils

carDetailUrl :: CarId -> Text
carDetailUrl carid = "/car/" <> showId carid

carOccupyNewUrl :: CarId -> Text
carOccupyNewUrl carid = (carDetailUrl carid) <> "/occupy/new"

occupationEditUrl :: OccupationId -> Text
occupationEditUrl occupid = "/occupation/" <> showId occupid <> "/edit"

reservationsUrl :: CarId -> Text
reservationsUrl carid = "/car/" <> showId carid <> "/reservation"

newReservationUrl :: CarId -> Text
newReservationUrl carid = "/car/" <> showId carid <> "/reservation/new"

editReservationUrl :: ReservationId -> Text
editReservationUrl resid = "/reservation/" <> showId resid <> "/edit"

gasNewUrl :: CarId -> Text
gasNewUrl carid = "/car/" <> showId carid <> "/gas/new"
