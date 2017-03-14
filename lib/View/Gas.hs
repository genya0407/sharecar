module View.Gas (
  gasNew_
) where

import Lucid
import Data.Text

import View.Type
import View.Widget
import Route.Url

import Utils

gasNew_ :: Me -> (Entity Car, Bool) -> Text
gasNew_ me (carEntity@(Entity carid car), isOccupied) = layout (Just me) $ do
  carViewWithOccupied_ carEntity isOccupied
  formGasAmount_ carid

formGasAmount_ :: CarId -> Html ()
formGasAmount_ carid = do
  form_ [action_ (gasNewUrl carid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    label_ [for_ "amount"] "ガス代"
    input_ [type_ "number", name_ "amount"]
    input_ [type_ "submit", value_ "確定"]
