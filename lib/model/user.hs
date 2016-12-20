{-# LANGUAGE OverloadedStrings #-}
module Model.User where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Text
import qualified Model.Session as Session

all :: MonadIO m => m [Entity User]
all = runDB $ selectList [] []

login :: MonadIO m => Text -> Text -> m (Maybe SessionId)
login email password = do
      mUser <- runDB $ selectList [UserMail ==. email] [LimitTo 1]
      case mUser of
        [Entity userid user] -> do
          sid <- Session.create userid
          return $ Just sid
        _ -> return Nothing
