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
  div_ [class_ "row"] $ do
    div_ [class_ "col s6 m6"] $ do
      label_ [for_ "begin-date"] "開始日"
      input_ [type_ "date", name_ "begin-date", id_ "begin-date", class_ "datepicker", value_ beginDate]
    div_ [class_ "col s6 m6"] $ do
      label_ [for_ "begin-time"] "開始時刻"
      input_ [type_ "time", name_ "begin-time", id_ "begin-time", value_ beginTime]
  div_ [class_ "row"] $ do
    div_ [class_ "col s6 m6"] $ do
      label_ [for_ "end-date"] "終了日"
      input_ [type_ "date", name_ "end-date", id_ "end-date", class_ "datepicker", value_ endDate]
    div_ [class_ "col s6 m6"] $ do
      label_ [for_ "end-time"] "終了時刻"
      input_ [type_ "time", name_ "end-time", id_ "end-time", value_ endTime]

layout :: Maybe Me -> Html () -> Text
layout mMe content = toStrict . renderText $ do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.8/css/materialize.min.css"]
      link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]
    body_ $ do
      header_ $ do
        case mMe of
          Just (Entity meid me) -> span_ . toHtml $ userName me
          Nothing -> ""
      div_ [class_ "container"] content
      script_ [src_ "https://code.jquery.com/jquery-2.1.1.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.8/js/materialize.min.js"] ("" :: Text)

carViewWithOccupied_ :: Entity Car -> Bool -> Html ()
carViewWithOccupied_ (Entity carid car) isOccupied = do
  i_ [class_ "material-icons", style_ "color: black"] "directions_car"
  if isOccupied then
    span_ [class_ "badge red white-text"] "使用中"
  else
    span_ [class_ "badge green white-text"] "使用可"
  toHtml $ carName car
