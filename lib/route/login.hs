{-# LANGUAGE OverloadedStrings #-}

module Route.Login where

import           Web.Spock hiding (SessionId)
import qualified View.Default as V
import qualified Model.User as User

loginRoute redirectAfterAuth = do
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
              redirect redirectAfterAuth
            Nothing -> text "login failed."
        _ -> text "login failed."
