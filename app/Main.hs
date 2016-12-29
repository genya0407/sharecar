{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock hiding (SessionId)
import Web.Spock.Config

import System.Environment (getArgs)
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
import qualified View as V
import qualified Form as F
import           Database.Persist (Entity(..))
import           Database.Persist.Sql (toSqlKey)
import           Data.List (intersperse)

import           Data.HVect

import qualified Route.Login as RL
import Utils

type SessionVal = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)
type AuthHook = ActionCtxT (HVect '[]) (WebStateM () (Maybe SessionId) MyAppState) (HVect ((Entity User) ': '[]))

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg Nothing PCNoDatabase (DummyAppState ref)
       args <- getArgs
       let ah = case args of
                  ["devel"] -> develHook
                  _ -> authHook
       runSpock 8080 (spock spockCfg (app ah))

authHook :: AuthHook
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

develHook :: AuthHook
develHook = do
  oldCtx <- getContext
  users <- User.all
  let user = (users Prelude.!! 0)
  return (user :&: oldCtx)

app :: AuthHook -> SpockCtxM () () SessionVal MyAppState ()
app ah = do
  prehook (return HNil) $ do
    get "/" $ redirect "/car"
    RL.loginRoute "/car"
    prehook ah $ do
      get "car" $ do
        me@(Entity meid _) <- liftM findFirst getContext
        carsWithOccupied <- Car.allWithOccupied
        occupsNotMeterEndByMe <- Occup.notMeterEndBy meid
        occupsAndCarsNotMeterEndByMe <- forM occupsNotMeterEndByMe $ \occupEntity@(Entity occupid occup) -> do
          Just car <- Car.find $ occupationCarId occup
          return (occupEntity, Entity (occupationCarId occup) car)
        html $ V.carIndex_ me carsWithOccupied occupsAndCarsNotMeterEndByMe
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
        Entity meid me <- liftM findFirst getContext
        mOccup <- F.formOccupationBegin meid carid
        case mOccup of
          Just occup -> do
            Occup.create occup
            redirect "/car"
          Nothing -> do
            redirect . T.pack $ "/car/" ++ show _carid ++ "/occupy/new"
      get ("occupation" <//> var <//> "edit") $ \_occupid -> do
        let occupid = toSqlKey _occupid
        (meEntity@(Entity meid me) :: Entity User) <- liftM findFirst getContext
        mOccup <- Occup.find occupid
        case mOccup of
          Just occup -> do
            if occupationUserId occup == meid then do
              mWithOccupied <- Car.withOccupied $ occupationCarId occup
              case mWithOccupied of
                Just withOccupied -> html $ V.occupationEdit meEntity withOccupied (Entity occupid occup)
                Nothing -> redirect "/car"
            else
              redirect "/car"
          Nothing -> redirect "/car"
      post ("occupation" <//> var <//> "edit") $ \_occupid -> do
        let occupid = toSqlKey _occupid
        (meEntity@(Entity meid me) :: Entity User) <- liftM findFirst getContext
        mOccup <- Occup.find occupid
        case mOccup of
          Just occup -> do
            if occupationUserId occup == meid then do
              mMeterEnd <- param "meter-end"
              case mMeterEnd of
                Just meterEnd -> do
                  Occup.replace occupid $ occup { occupationMeterEnd = Just meterEnd }
                  redirect "/car"
                Nothing -> redirect "/car"
            else
              redirect "/car"
          Nothing -> redirect "/car"
