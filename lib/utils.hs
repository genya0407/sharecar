module Utils where
import Data.Time.LocalTime
import Data.Time.Clock

addZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
addZonedTime df t = utcToZonedTime (unsafePerformIO getCurrentTimeZone) $ addUTCTime df (zonedTimeToUTC t)
