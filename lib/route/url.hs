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

carReservationsUrl :: CarId -> Text
carReservationsUrl carid = "/car/" <> showId carid <> "reservation"

gasNewUrl :: CarId -> Text
gasNewUrl carid = "/car/" <> showId carid <> "/gas/new"
