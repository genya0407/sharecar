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
import qualified View.Default as V
import           Database.Persist (Entity(..))

import           Data.HVect

type SessionVal = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg Nothing PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

--initHook :: MonadIO m => ActionCtxT () m (HVect '[])
initHook = return HNil

--authHook :: ActionCtxT (HVect xs) m (HVect ((Entity User) ': xs))
authHook = do
  oldCtx <- getContext
  mSid <- readSession
  case mSid of
    Just sid -> do
      mUser <- User.getBySid sid
      case mUser of
        Nothing -> text "Sorry, no access!"
        Just user -> return (user :&: oldCtx)
    _ -> text "Do login!"

app :: SpockCtxM () () SessionVal MyAppState ()
app = do
  prehook initHook $ do
    get "login" $ do
      html $ V.login_
    post "login" $ do
      mEmail <- param "email" 
      mPassword <- param "password"
      case (mEmail, mPassword) of
        (Just email, Just password) -> do
          mSid <- User.login email password
          case mSid of
            Just sid -> do
              writeSession (Just sid)
              redirect "/users"
            Nothing -> text "login failed."
        _ -> text "login failed."
    prehook authHook $ do
      get root $ text "Hello World!"
      get "users" $ do
        -- (me :: Entity User) <- liftM findFirst getContext
        users <- User.all
        html $ V.users_ users
      get "reservations" $ do
        reservs <- Reserv.active
        html $ V.reservations_ reservs

