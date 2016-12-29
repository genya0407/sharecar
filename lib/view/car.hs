
module View.Car where

import Database.Persist (Entity(..))
import Data.Text
import Data.Time.Clock
import Lucid
import Control.Monad

import View.Type
import View.Widget
import Model.Type
import Route.Url

carIndex_ :: Me -> [(Entity Car, Bool)] -> [(Entity Occupation, Entity Car)] -> Text
carIndex_ me carsWithOccupied occupsAndCarsNotMeterEndByMe = layout (Just me) $ do
  ul_ $ do
    forM_ occupsAndCarsNotMeterEndByMe $ \(occupsEntity@(Entity occupid occup), carEntity@(Entity carid car)) -> do
      li_ $ a_ [href_ (occupationEditUrl occupid)] $ do
        toHtml $ carName car
        ": "
        toHtml $ (utctDay . occupationBegin $ occup)
  hr_ []
  ul_ $ do
    forM_ carsWithOccupied $ \(carEntity@(Entity carid car), isOccupied) -> do
      li_ $ do
        a_ [href_ (carDetailUrl carid)] $ carViewWithOccupied_ carEntity isOccupied

carDetail_ :: Me -> (Entity Car, Bool) -> Text
carDetail_ me (carEntity@(Entity carid car), isOccupied) = layout (Just me) $ do
  carViewWithOccupied_ carEntity isOccupied
  ul_ $ do
    li_ $ a_ [href_ (carOccupyNewUrl carid)] "使用"
    li_ "予約"
    li_ "ガス代"
    li_ "使用履歴"
