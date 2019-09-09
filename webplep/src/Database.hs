module Database where

import Web.Spock
import Database.Persist.Sqlite hiding (get)
import           Database.Persist              hiding (get)
import Control.Monad.Logger 
import Data.Text (Text)
import qualified Data.Text as T

import Model
import TypeDefinitions

runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSql action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

allNotes :: SqlPersistT (LoggingT IO) [Note]
allNotes = map entityVal <$> selectList [] []

addUser :: User -> SqlPersistT (LoggingT IO) CommonResponse
addUser user = do insert user
                  return (CommonSuccess "hello")