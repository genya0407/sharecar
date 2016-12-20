{-# LANGUAGE OverloadedStrings          #-}

import           Database.Persist
import           Database.Persist.Sqlite
import           Model.Type
import           Control.Monad.IO.Class

main :: IO ()
main = do
  runSqlite "db/db.sqlite3" $ do
    runMigration migrateAll
