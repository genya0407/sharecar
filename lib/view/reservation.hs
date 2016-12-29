module View.Reservation (
  reservations_,
  newReservation_
)where

import Lucid
import Data.Text

import View.Type
import View.Widget
import Route.Url

import Utils

reservations_ :: Me -> (Entity Car, Bool) -> [(Entity Reservation, Entity User)] -> Text
reservations_ me (carEntity, isOccupied) reservations = layout (Just me) $ do
  carViewWithOccupied_ carEntity isOccupied
  hr_ []
  ul_ $ mapM_ reservation_ reservations
  br_ []
  a_ [href_ (newReservationUrl $ entityKey carEntity)] "予約する"

reservation_ :: (Entity Reservation, Entity User) -> Html ()
reservation_ ((Entity resid res), (Entity userid user)) = do
  li_ $ toHtml (userName user) <> ": " <> begin <> "〜" <> end
    where
      begin = toHtml $ reservationBegin res
      end = toHtml $ reservationEnd res

newReservation_ :: Me -> CarId -> Text
newReservation_ me carid = layout (Just me) $ do
  form_ [action_ (newReservationUrl carid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    inputBeginEnd_ Nothing Nothing
    input_ [type_ "submit", value_ "予約"]
