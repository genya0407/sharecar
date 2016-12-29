module View.Widget where

import Data.Maybe
import Data.Text
import Data.Text.Lazy (toStrict)
import Lucid

import View.Type

import Utils

inputBeginEnd_ :: Maybe UTCTime -> Maybe UTCTime -> Html ()
inputBeginEnd_ mBegin mEnd = do
  let
    beginDate = pack $ fromMaybe "" $ formatTime defaultTimeLocale "%F" <$> mBegin
    beginTime = pack $ fromMaybe "" $ formatTime defaultTimeLocale "%R" <$> mBegin
    endDate = pack $ fromMaybe "" $ formatTime defaultTimeLocale "%F" <$> mEnd
    endTime = pack $ fromMaybe "" $ formatTime defaultTimeLocale "%R" <$> mEnd
  label_ [for_ "begin"] "時刻"
  input_ [type_ "date", name_ "begin-date", value_ beginDate]
  input_ [type_ "time", name_ "begin-time", value_ beginTime]
  "〜"
  input_ [type_ "date", name_ "end-date", value_ endDate]
  input_ [type_ "time", name_ "end-time", value_ endTime]

layout :: Maybe Me -> Html () -> Text
layout mMe content = toStrict . renderText $ do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    body_ $ do
      header_ $ do
        case mMe of
          Just (Entity meid me) -> span_ . toHtml $ userName me
          Nothing -> ""
      content


carViewWithOccupied_ :: Entity Car -> Bool -> Html ()
carViewWithOccupied_ (Entity carid car) isOccupied = do
  div_ $ do
    span_ . toHtml $ carName car
    span_ $ if isOccupied then
              "使用中"
            else
              "使用可"

