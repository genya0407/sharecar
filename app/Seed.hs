{-# LANGUAGE OverloadedStrings #-}

import           Model.Type
import qualified Model.User as User
import qualified Model.Car as Car
import qualified Model.Reservation as Reserv
import qualified Model.Occupation as Occup

import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.Random (randomRIO)
import           Control.Monad

main = do
  user1 <- User.create "example1@example.com" "Yamada Taro" "0123456789" "password1"
  user2 <- User.create "example2@example.com" "Yamada Jiro" "0123456789" "password2"
  user3 <- User.create "example3@example.com" "Yamada Saburo" "0123456789" "password3"

  car1 <- Car.create "ミラ"
  car2 <- Car.create "ステップワゴン"
  car3 <- Car.create "ライフ"
  car4 <- Car.create "オデッセイ"

  now <- return . zonedTimeToUTC =<< getZonedTime
  forM_ [(user1, car2), (user3 ,car4), (user2, car1)] $ \(user, car) -> do
    startPoint <- randomRIO (0, 24 * 60 * 60 * 14)
    reserveLength <- randomRIO (60 * 60 * 3, 24 * 60 * 60 * 14)
    let
      begin = addUTCTime (fromInteger startPoint) now
      end = addUTCTime (fromInteger reserveLength) begin
    Reserv.create user car begin end
