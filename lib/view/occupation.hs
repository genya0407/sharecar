module View.Occupation (
  occupationEdit_,
  occupyNew_
) where

import Data.Text
import Data.Time.Clock
import Data.Maybe
import Model.Type
import View.Type
import View.Widget
import Route.Url
import Utils
import Lucid
import Database.Persist (Entity(..))

occupationEdit_ :: Me -> (Entity Car, Bool) -> Entity Occupation -> Text
occupationEdit_ me (carEntity@(Entity carid car), isOccupied) (Entity occupid occup) = layout (Just me) $ do
  carViewWithOccupied_ carEntity isOccupied
  "時刻" >> (toHtml $ occupationBegin occup) >> "〜" >> (toHtml $ occupationEnd occup)
  br_ []
  "メーター" >> (toHtml . show $ occupationMeterBegin occup) >> "〜"
  form_ [action_ (occupationEditUrl occupid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    input_ [type_ "number", name_ "meter-end"]
    input_ [type_ "submit", value_ "送信"]

occupyNew_ :: Me -> (Entity Car, Bool) -> Maybe UTCTime -> Maybe Int -> Text
occupyNew_ me (carEntity@(Entity carid car), isOccupied) mNow mMeterBegin = layout (Just me) $ do
  form_ [action_ (carOccupyNewUrl carid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    carViewWithOccupied_ carEntity isOccupied
    inputBeginEnd_ mNow mNow
    inputMeterBegin_ mMeterBegin
    input_ [type_ "submit", value_ "使用開始"]

inputMeterBegin_ :: Maybe Int -> Html ()
inputMeterBegin_ mMeterBegin = do
  let meterBegin' = fromMaybe "" (pack . show <$> mMeterBegin)
  label_ [for_ "meter-begin"] "乗車時メーター"
  input_ [type_ "number", name_ "meter-begin", value_ meterBegin']
