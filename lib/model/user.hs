{-# LANGUAGE OverloadedStrings #-}
module Model.User where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.ByteString
import qualified Model.Session as Session
import           Crypto.Hash.SHA1 (hash)

all :: MonadIO m => m [Entity User]
all = runDB $ selectList [] []

type Name = Text
type Email = Text
type PhoneNumber = Text
type Password = ByteString

create :: MonadIO m => Email -> Name -> PhoneNumber -> Password -> m (Key User)
create email name phoneNumber password = do
  let
    cryptPassword = hash password
    user = User
      { userMail = email
      , userName = name
      , userPhoneNumber = phoneNumber
      , userCryptPassword = cryptPassword
      }
  runDB $ insert user

getBySid :: MonadIO m => SessionId -> m (Maybe (Entity User))
getBySid sid = do
  Just (Entity _ session) <- Session.find sid
  Just user <- runDB $ get (sessionUserId session)
  return . Just $ Entity (sessionUserId session) user

login :: MonadIO m => Text -> Text -> m (Maybe SessionId)
login email password = do
      mUser <- runDB $ selectList [UserMail ==. email] [LimitTo 1]
      case mUser of
        [Entity userid user] -> do
          if userCryptPassword user == (hash . encodeUtf8) password then do
            sid <- Session.create userid
            return $ Just sid
          else
            return Nothing
        _ -> return Nothing
