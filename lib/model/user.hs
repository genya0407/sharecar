module Model.User where

import           Model.Type
import           Database.Persist
import           Control.Monad.IO.Class

all :: MonadIO m => m [Entity User]
all = runDB $ selectList [] []
