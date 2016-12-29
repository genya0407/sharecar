{-# LANGUAGE OverloadedStrings #-}

import           System.Random (randomRIO)

import qualified Model.User as User
import qualified Model.Car as Car
import qualified Model.Reservation as Reserv
import qualified Model.Occupation as Occup

import           Utils

main = do
  user1 <- User.createCrypt "example1@example.com" "Yamada Taro" "0123456789" "password1"
  user2 <- User.createCrypt "example2@example.com" "Yamada Jiro" "0123456789" "password2"
  user3 <- User.createCrypt "example3@example.com" "Yamada Saburo" "0123456789" "password3"

  car1 <- Car.create $ Car.new { carName = "ミラ" }
  car2 <- Car.create $ Car.new { carName = "ステップワゴン" }
  car3 <- Car.create $ Car.new { carName = "ライフ" }
  car4 <- Car.create $ Car.new { carName = "オデッセイ" }

  now <- getCurrentTime'
  forM_ [(user, car) | user <- [user1, user2, user3], car <- [car1, car2, car3, car4]] $ \(user, car) -> do
    startPoint <- randomRIO (0, 24 * 60 * 60 * 14)
    reserveLength <- randomRIO (60 * 60 * 3, 24 * 60 * 60 * 14)
    let
      begin = addUTCTime (fromInteger startPoint) now
      end = addUTCTime (fromInteger reserveLength) begin
    Reserv.create $ Reserv.new
      { reservationUserId = user
      , reservationCarId = car
      , reservationBegin = begin
      , reservationEnd = end }
