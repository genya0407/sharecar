{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Spock hiding (SessionId)
import Web.Spock.Config

import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import Data.IORef
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T

import           Model.Type
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Model.Reservation as Reserv
import qualified Model.Occupation as Occup
import qualified Model.Car as Car
import qualified View.Default as V
import           Database.Persist (Entity(..))
import           Database.Persist.Sql (toSqlKey)
import           Data.List (intersperse)

import           Data.HVect

import qualified Route.Login as RL
import Utils

type SessionVal = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg Nothing PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

--authHook :: ActionCtxT (HVect xs) m (HVect ((Entity User) ': xs))
authHook = do
  oldCtx <- getContext
  mSid <- readSession
  case mSid of
    Just sid -> do
      mUser <- User.getBySid sid
      case mUser of
        Nothing -> redirect "/login"
        Just user -> return (user :&: oldCtx)
    _ -> redirect "/login"

develHook = do
  oldCtx <- getContext
  --mSid <- readSession
  users <- User.all
  let user = (users Prelude.!! 0)
  return (user :&: oldCtx)

app :: SpockCtxM () () SessionVal MyAppState ()
app = do
  prehook (return HNil) $ do
    RL.loginRoute "/car"
    prehook develHook $ do -- Caution !!!
      get "car" $ do
        me <- liftM findFirst getContext
        carsWithOccupied <- Car.allWithOccupied
        html $ V.carIndex_ me carsWithOccupied
      get ("car" <//> var) $ \_carid -> do
        let carid = toSqlKey _carid
        me <- liftM findFirst getContext
        mCarWithOccupied <- Car.withOccupied carid
        case mCarWithOccupied of
          Just carWithOccupied -> html $ V.carDetail_ me carWithOccupied
          Nothing -> redirect "/car"
      get ("car" <//> var <//> "occupy/new") $ \_carid -> do
        let carid = toSqlKey _carid
        (me :: Entity User) <- liftM findFirst getContext
        mCarWithOccupied <- Car.withOccupied carid
        case mCarWithOccupied of
          Just carWithOccupied@(Entity carid car, isOccupied) -> do
            now <- liftIO getCurrentTime'
            mLastOccup <- Occup.lastByMeter carid
            html $ V.carOccupyNew_ me carWithOccupied (Just now) (occupationMeterEnd =<< (\(Entity occupid occup) -> Just occup) =<< mLastOccup)
          Nothing -> redirect "/car"
      post ("car" <//> var <//> "occupy/new") $ \_carid -> do
        let carid = toSqlKey _carid
        (meElement@(Entity meid me) :: Entity User) <- liftM findFirst getContext
        mOccup <- formOccupationBegin meid carid
        case mOccup of
          Just occup -> do
            Occup.save occup
            redirect "/car"
          Nothing -> do
            redirect "/car"

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
