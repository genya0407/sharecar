{-# LANGUAGE OverloadedStrings #-}

module Form (
  formOccupationBegin,
  formGas, formReservation
)where

import           Web.Spock
import qualified Model.Occupation as Occup
import qualified Model.Gas as Gas
import qualified Model.Reservation as Reserv
import           Utils

formOccupationBegin :: MonadIO m => UserId -> CarId -> ActionCtxT ctx m (Maybe Occupation)
formOccupationBegin userid carid = do
  mBeginAndEnd <- getBeginAndEnd
  mMeterBegin <- param "meter-begin"
  return $ do
    (begin, end) <- mBeginAndEnd
    meterBegin <- mMeterBegin
    return $ Occup.new
      { occupationUserId = userid
      , occupationCarId = carid
      , occupationBegin = begin
      , occupationEnd = end
      , occupationMeterBegin = meterBegin
      }

formGas :: MonadIO m => UserId -> CarId -> ActionCtxT ctx m (Maybe Gas)
formGas userid carid = do
  mAmount <- param "amount"
  return $ do
    amount <- mAmount
    return $ Gas.new
      { gasUserId = userid
      , gasCarId = carid
      , gasAmount = amount }

formReservation :: MonadIO m => UserId -> CarId -> ActionCtxT ctx m (Maybe Reservation)
formReservation userid carid = do
  mBaE <- getBeginAndEnd
  return $ do
    (begin, end) <- mBaE
    return $ Reserv.new
      { reservationBegin = begin
      , reservationEnd = end
      , reservationUserId = userid
      , reservationCarId = carid }

getBeginAndEnd :: MonadIO m => ActionCtxT ctx m (Maybe (UTCTime, UTCTime))
getBeginAndEnd = do
  mBeginDate <- param "begin-date"
  mBeginTime <- param "begin-time"
  mEndDate <- param "end-date"
  mEndTime <- param "end-time"
  return $ do
    begin <- parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mBeginDate <*> mBeginTime)
    end <- parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mEndDate <*> mEndTime)
    return (begin, end)
