module Database where

import Web.Spock hiding (head)
import Database.Persist.Sqlite hiding (get)
import           Database.Persist              hiding (get)
import Control.Monad.Logger 
import Data.Text (Text)
import qualified Data.Text as T

import Model
import TypeDefinitions

-- | Run a query on our database.
runSql :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSql action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

-- | Retrieves all notes from the database.
--
-- >>> runSql allNotes
allNotes :: SqlPersistT (LoggingT IO) [Note]
allNotes = map entityVal <$> selectList [] []

allNotesFromUser :: Text -> SqlPersistT (LoggingT IO) [Note]
allNotesFromUser username = map entityVal <$> selectList [NoteAuthor <-. [username]] []

-- | Retrieves all users from the database.
--
-- >>> runSql allUsers
allUsers :: SqlPersistT (LoggingT IO) [User]
allUsers = map entityVal <$> selectList [] []

-- | Add a user to the database. TODO hash password before storing.
-- 
-- >>> runSql $ addUser user
addUser :: User -> SqlPersistT (LoggingT IO) CommonResponse
addUser user = do insert user
                  return (CommonSuccess "User has been added to the database.")

-- | Gets all user from the database with this username. 
--
-- >>> runSql $ getUserByName username
getUserByName :: Text -> SqlPersistT (LoggingT IO) [User]
getUserByName username = map entityVal <$> selectList [UserUsername <-. [username]] []

-- | Gets all users from the database with this username and (hashed) password.
-- Returns an empty list when this combination does not exist in the database.
getUserByCredentials :: User -> SqlPersistT (LoggingT IO) [User]
getUserByCredentials user = map entityVal <$> selectList [UserUsername <-. [userUsername user], UserPassword <-. [userPassword user]] []