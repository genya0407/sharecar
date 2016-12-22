module Utils where
import Data.Time.Clock

getCurrentTime' :: IO UTCTime
getCurrentTime' = do
  now <- getCurrentTime
  return $ addUTCTime (60 * 60 * 9) now
