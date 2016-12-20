module Model.Occupation where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class

all :: MonadIO m => m [Entity Occupation]
all = runDB $ selectList [] []
