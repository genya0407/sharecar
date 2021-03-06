{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Web.Spock hiding (SessionId)
import           Web.Spock.Config

import           System.Environment (getArgs)
import           Data.Monoid
import           Data.IORef
import           Data.List (intersperse)
import           Data.HVect
import qualified Data.Text as T
import           Network.Wai.Middleware.Static

import qualified Model.User as User
import qualified Model.Session as Session
import qualified Model.Reservation as Reserv
import qualified Model.Occupation as Occup
import qualified Model.Car as Car
import qualified View as V
import qualified Form as F
import qualified Route.Login as RL
import qualified Route.Reservation as RR
import qualified Route.Occupation as RO
import qualified Route.Gas as RG
import           System.Environment (getEnv)

import           Utils

type AuthHook = ActionCtxT (HVect '[]) (WebStateM () (Maybe SessionId) MyAppState) (HVect ((Entity User) ': '[]))

main :: IO ()
main =
    do ref <- newIORef 0
       port <- getEnv "SHARECAR_PORT"
       spockCfg <- defaultSpockCfg Nothing PCNoDatabase (DummyAppState ref)
       args <- getArgs
       let ah = case args of
                  ["devel"] -> develHook
                  _ -> authHook
       runSpock (read port) (spock spockCfg (app ah))

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
      middleware (staticPolicy (addBase "static"))
      RO.occupationRoute
      RG.gasRoute
      RR.reservationRoute
      get "car" $ do
        me@(Entity meid _) <- liftM findFirst getContext
        carsWithOccupied <- Car.allWithOccupied
        occupsNotMeterEndByMe <- Occup.notMeterEndBy meid
        occupsAndCarsNotMeterEndByMe <- forM occupsNotMeterEndByMe $ \occupEntity@(Entity occupid occup) -> do
          Just car <- Car.find $ occupationCarId occup
          return (occupEntity, Entity (occupationCarId occup) car)
        html $ V.index_ me carsWithOccupied occupsAndCarsNotMeterEndByMe
      get ("car" <//> var) $ \_carid -> do
        let carid = toSqlKey _carid
        me <- liftM findFirst getContext
        mCarWithOccupied <- Car.withOccupied carid
        case mCarWithOccupied of
          Just carWithOccupied -> html $ V.carDetail_ me carWithOccupied
          Nothing -> redirect "/car"
