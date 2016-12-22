module Model.Session where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Time.Clock
import           Utils

all :: MonadIO m => m [Entity Session]
all = runDB $ selectList [] [] 

create :: MonadIO m => UserId -> m SessionId
create userid = do
  now <- liftIO $ getCurrentTime'
  let until = addUTCTime (60 * 24 * 14) now
  runDB . insert $ Session { sessionValidUntil = until, sessionUserId = userid }

find :: MonadIO m => SessionId -> m (Maybe (Entity Session))
find sid = do
  sessions <- runDB $ selectList [SessionId ==. sid] [LimitTo 1]
  case sessions of
    [session] -> return . Just $ session
    _ -> return Nothing
