module Database where

import Web.Spock
import Database.Persist.Sqlite
import Control.Monad.Logger 

import Model

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSql action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

allNotes :: SqlPersistT (LoggingT IO) [Note]
allNotes = map entityVal <$> selectList [] []