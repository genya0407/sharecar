module Utils (
  module Utils,
  toSqlKey, fromSqlKey
)where
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad.IO.Class
import Database.Persist.Sql

getCurrentTime' :: MonadIO m => m UTCTime
getCurrentTime' = do
  now <- liftIO getCurrentTime
  return $ addUTCTime (60 * 60 * 9) now

defaultUTCTime = UTCTime (ModifiedJulianDay 0) 0
