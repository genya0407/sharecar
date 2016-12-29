{-# LANGUAGE OverloadedStrings #-}

module Form where

import           Web.Spock
import qualified Model.Occupation as Occup
import qualified Model.Gas as Gas
import           Utils

formOccupationBegin :: MonadIO m => UserId -> CarId -> ActionCtxT ctx m (Maybe Occupation)
formOccupationBegin userid carid = do
  mBeginDate <- param "begin-date"
  mBeginTime <- param "begin-time"
  mEndDate <- param "end-date"
  mEndTime <- param "end-time"
  mMeterBegin <- param "meter-begin"
  return $ do
    begin <- parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mBeginDate <*> mBeginTime)
    end <- parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mEndDate <*> mEndTime)
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
