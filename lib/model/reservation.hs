module Model.Reservation where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class

all :: MonadIO m => m [Entity Reservation]
all = runDB $ selectList [] []
