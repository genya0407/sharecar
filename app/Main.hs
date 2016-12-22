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
import qualified Data.Text as T

import           Model.Type
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Model.Reservation as Reserv
import qualified Model.Occupation as Occup
import qualified Model.Car as Car
import qualified View.Default as V
import           Database.Persist (Entity(..))

import           Data.HVect

import qualified Route.Login as RL

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

app :: SpockCtxM () () SessionVal MyAppState ()
app = do
  prehook (return HNil) $ do
    RL.loginRoute "/"
    prehook authHook $ do
      get root $ do
        carsWithOccupied <- Car.allWithOccupied
        html $ V.root_ carsWithOccupied
