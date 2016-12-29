{-# LANGUAGE OverloadedStrings #-}

module Form where

import           Web.Spock
import           Model.Type
import qualified Model.Occupation as Occup
import           Control.Monad.IO.Class
import           Data.Time.Format

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
