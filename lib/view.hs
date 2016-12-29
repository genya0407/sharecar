{-# LANGUAGE OverloadedStrings #-}

module View where

import Lucid
import Data.Text.Lazy (toStrict)
import Data.Text
import Model.Type
import Database.Persist
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Control.Monad
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe

type Me = Entity User

carDetailUrl :: CarId -> Text
carDetailUrl carid = "/car/" <> (pack . show . fromSqlKey $ carid)

carOccupyNewUrl :: CarId -> Text
carOccupyNewUrl carid = (carDetailUrl carid) <> "/occupy/new"

occupationEditUrl :: OccupationId -> Text
occupationEditUrl occupid = "/occupation/" <> ( pack . show . fromSqlKey $ occupid) <> "/edit"

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

occupationEdit :: Me -> (Entity Car, Bool) -> Entity Occupation -> Text
occupationEdit me (carEntity@(Entity carid car), isOccupied) (Entity occupid occup) = layout (Just me) $ do
  carViewWithOccupied_ carEntity isOccupied
  "時刻" >> (toHtml $ occupationBegin occup) >> "〜" >> (toHtml $ occupationEnd occup)
  br_ []
  "メーター" >> (toHtml . show $ occupationMeterBegin occup) >> "〜"
  form_ [action_ (occupationEditUrl occupid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    input_ [type_ "number", name_ "meter-end"]
    input_ [type_ "submit", value_ "送信"]

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

inputMeterBegin_ :: Maybe Int -> Html ()
inputMeterBegin_ mMeterBegin = do
  let meterBegin' = fromMaybe "" (pack . show <$> mMeterBegin)
  label_ [for_ "meter-begin"] "乗車時メーター"
  input_ [type_ "number", name_ "meter-begin", value_ meterBegin']

carOccupyNew_ :: Me -> (Entity Car, Bool) -> Maybe UTCTime -> Maybe Int -> Text
carOccupyNew_ me (carEntity@(Entity carid car), isOccupied) mNow mMeterBegin = layout (Just me) $ do
  form_ [action_ (carOccupyNewUrl carid), method_ "POST", acceptCharset_ "UTF-8"] $ do
    carViewWithOccupied_ carEntity isOccupied
    inputBeginEnd_ mNow mNow
    inputMeterBegin_ mMeterBegin
    input_ [type_ "submit", value_ "使用開始"]

carViewWithOccupied_ :: Entity Car -> Bool -> Html ()
carViewWithOccupied_ (Entity carid car) isOccupied = do
  div_ $ do
    span_ . toHtml $ carName car
    span_ $ if isOccupied then
              "使用中"
            else
              "使用可"

login_ :: Text
login_ = layout Nothing $ do
  h1_ "Login"
  form_ [action_ "/login", method_ "POST"] $ do
    label_  $ do
      "Email: "
      input_ [type_ "email", name_ "email"]
    br_ []
    label_  $ do
      "Password: "
      input_ [type_ "password", name_ "password"]
    br_ []
    input_ [type_ "submit", value_ "Login"]

users_ :: [Entity User] -> Text
users_ users = layout Nothing $ do
  forM_ users $ \(Entity userid user) -> do
    ul_ $ do
      li_ . toHtml $ userid
      li_ . toHtml $ userName user
      li_ . toHtml $ userPhoneNumber user

reservations_ :: [Entity Reservation] -> Text
reservations_ reservs = layout Nothing $ do
  forM_ reservs $ \(Entity reservid reserv) -> do
    ul_ $ do
      li_ . toHtml $ reservid
      li_ . toHtml $ reservationUserId reserv
      li_ . toHtml $ reservationCarId reserv
      li_ . toHtml $ reservationBegin reserv
      li_ . toHtml $ reservationEnd reserv
