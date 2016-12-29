module View.Reservation (
  reservations_
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

reservation_ :: (Entity Reservation, Entity User) -> Html ()
reservation_ ((Entity resid res), (Entity userid user)) = do
  li_ $ toHtml (userName user) <> ": " <> begin <> "〜" <> end
    where
      begin = toHtml $ reservationBegin res
      end = toHtml $ reservationEnd res
