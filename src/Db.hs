module Db where

import Control.Exception
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

dbName = "finger.db"
-- Takes a function that requires a connection
-- and invokes that function with "finger.db" connection
withDb :: (SQLite.Connection -> IO a) -> IO a
withDb =
  bracket
    (SQLite.open dbName)
    SQLite.close
