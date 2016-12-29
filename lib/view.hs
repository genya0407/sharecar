{-# LANGUAGE OverloadedStrings #-}

module View (
  module View,
  module View.Car,
  module View.Occupation,
  module View.Gas,
  module View.Reservation
)where

import Lucid
import Data.Text
import Database.Persist
import Data.Maybe

import Route.Url
import View.Type
import View.Widget
import View.Car
import View.Occupation
import View.Gas
import View.Reservation

import Utils

index_ = carIndex_

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
