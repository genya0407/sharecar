{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock hiding (SessionId)
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import           Model.Type
import qualified Model.User as User
import qualified Model.Session as Session
import qualified View.Default as V
import           Database.Persist (Entity(..))

type SessionVal = Maybe SessionId
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg Nothing PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () SessionVal MyAppState ()
app = do
  get root $ text "Hello World!"
  get "users" $ do
    mSid <- readSession
    case mSid of
      Just sid -> do
        users <- liftIO $ User.all
        -- html $ V.users_ users
        Just (Entity userid user) <- User.getBySid sid
        text $ userName user
      Nothing -> text "ログインしてください"
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
          Nothing -> text "ログインに失敗しました。"
      _ -> text "入力されていない項目があります。"
