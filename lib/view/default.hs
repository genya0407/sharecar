{-# LANGUAGE OverloadedStrings #-}

module View.Default where

import Lucid
import Data.Text.Lazy (toStrict)
import Data.Text
import Model.Type
import Database.Persist
import Database.Persist.Sql
import Control.Monad
import Data.Monoid
import Data.Time.Clock

layout :: Html () -> Text
layout content = toStrict . renderText $ do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    body_ content

root_ :: [(Entity Car, Bool)] -> Text
root_ carsWithOccupied = layout $ do
  ul_ $ do
    forM_ carsWithOccupied $ \(car, isOccupied) -> do
      li_ $ do
        carViewWithOccupied_ car isOccupied

carViewWithOccupied_ :: Entity Car -> Bool -> Html ()
carViewWithOccupied_ (Entity carid car) isOccupied = do
  div_ $ do
    span_ . toHtml $ carName car
    span_ $ if isOccupied then
              "使用中"
            else
              "使用可"

login_ :: Text
login_ = layout $ do
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
users_ users = layout $ do
  forM_ users $ \(Entity userid user) -> do
    ul_ $ do
      li_ . toHtml $ userid
      li_ . toHtml $ userName user
      li_ . toHtml $ userPhoneNumber user

reservations_ :: [Entity Reservation] -> Text
reservations_ reservs = layout $ do
  forM_ reservs $ \(Entity reservid reserv) -> do
    ul_ $ do
      li_ . toHtml $ reservid
      li_ . toHtml $ reservationUserId reserv
      li_ . toHtml $ reservationCarId reserv
      li_ . toHtml $ reservationBegin reserv
      li_ . toHtml $ reservationEnd reserv
