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
  let
    mBegin = parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mBeginDate <*> mBeginTime)
    mEnd = parseTimeM True defaultTimeLocale "%F/%R" =<< ((\d t -> d ++ "/" ++ t) <$> mEndDate <*> mEndTime)
  mMeterBegin <- param "meter-begin"
  let mOccupIO = Occup.new <$> Just userid
                           <*> Just carid
                           <*> mBegin
                           <*> mEnd
                           <*> mMeterBegin
                           <*> Just Nothing -- mMeterEndはnewのときは必ずNothing
  case mOccupIO of
    Just occupIO -> do
      occup <- occupIO
      return $ Just occup
    Nothing -> return Nothing
