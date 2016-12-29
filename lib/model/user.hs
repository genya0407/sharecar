{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.User where

import           Database.Persist
import           Data.Either
import           Data.Text hiding (find)
import           Data.Text.Encoding (encodeUtf8)
import           Data.ByteString hiding (find)
import qualified Model.Session as Session
import           Crypto.Hash.SHA1 (hash)

import           Template

import           Utils

mkBoilerplate "User"

new = User "" "" "" "" defaultUTCTime defaultUTCTime

type Name = Text
type Email = Text
type PhoneNumber = Text
type Password = ByteString

createCrypt :: MonadIO m => Email -> Name -> PhoneNumber -> Password -> m (Key User)
createCrypt email name phoneNumber password = do
  let
    cryptPassword = hash password
    user = new
      { userMail = email
      , userName = name
      , userCryptPassword = cryptPassword
      , userPhoneNumber = phoneNumber
      }
  create $ user

getBySid :: MonadIO m => SessionId -> m (Maybe (Entity User))
getBySid sid = do
  Just session <- Session.find sid
  Just user <- find (sessionUserId session)
  return . Just $ Entity (sessionUserId session) user

login :: MonadIO m => Text -> Text -> m (Maybe SessionId)
login email password = do
      mUser <- runDB $ selectList [UserMail ==. email] [LimitTo 1]
      case mUser of
        [Entity userid user] -> do
          if userCryptPassword user == (hash . encodeUtf8) password then do
            sid <- Session.create $ Session.new { sessionUserId = userid }
            return $ Just sid
          else
            return Nothing
        _ -> return Nothing
