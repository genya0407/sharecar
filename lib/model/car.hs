module Model.Car where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class

all :: MonadIO m => m [Entity Car]
all = runDB $ selectList [] []
