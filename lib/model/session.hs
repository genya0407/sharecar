module Model.Session where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Time.Clock

create :: MonadIO m => UserId -> m SessionId
create userid = do
  now <- liftIO $ getCurrentTime
  let until = addUTCTime (60 * 24 * 14) now
  runDB . insert $ Session { sessionValidUntil = until, sessionUserId = userid }
