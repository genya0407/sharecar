module Model.Car where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class
import           Data.Text

all :: MonadIO m => m [Entity Car]
all = runDB $ selectList [] []

create :: MonadIO m => Text -> m (Key Car)
create name = runDB . insert $ Car name
