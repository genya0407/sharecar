module View.Reservation (
  reservations_,
  newReservation_,
  editReservation_
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
  ul_ $ mapM_ (reservation_ me) reservations
  br_ []
  a_ [href_ (newReservationUrl $ entityKey carEntity)] "予約する"

reservation_ :: Me -> (Entity Reservation, Entity User) -> Html ()
reservation_ me ((Entity resid res), (Entity userid user)) = do
  let link = if (entityKey me) == userid then
               a_ [href_ (editReservationUrl resid)]
             else
               span_ []
  li_ . link  $ do
    toHtml (userName user) <> ": " <> begin <> "〜" <> end
    where
      begin = toHtml $ reservationBegin res
      end = toHtml $ reservationEnd res

newReservation_ :: Me -> CarId -> Text
newReservation_ me carid = layout (Just me) $ do
  form_ [action_ (newReservationUrl carid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    inputBeginEnd_ Nothing Nothing
    input_ [type_ "submit", value_ "予約"]

editReservation_ :: Me -> Entity Reservation -> Text
editReservation_ me (Entity resid res) = layout (Just me) $ do
  form_ [action_ (editReservationUrl resid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    inputBeginEnd_ (Just $ reservationBegin res) (Just $ reservationEnd res)
    input_ [type_ "submit", value_ "予約修正"]
