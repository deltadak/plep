module Actions.UserManagement
  ( registerAction
  , loginAction
  , logoutAction
  , getUserFromSession
  ) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.IORef
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Control.Monad                   (forM_, when)
import           Database.Persist.Sqlite hiding (get)
import           Web.Spock

import           Database
import           Model
import           TypeDefinitions
import Actions.UserAuthentication

-- | When registering, add a user to the database.
registerAction :: User -> PlepAction CommonResponse
registerAction user = runSql $ addUser user

-- | When logging in, check if this user is an existing user and if they are using the correct password to log in.
--   Then put the user in the current session.
loginAction :: User -> PlepAction ()
loginAction user = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  validUser <- validateUserAction user
  Control.Monad.when validUser $ liftIO $ modifyIORef' currentSessionRef $ M.insert sessionId (userUsername user)
  
-- | Remove the user from the session.
logoutAction :: PlepAction ()
logoutAction = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  liftIO $ modifyIORef' currentSessionRef $ M.delete sessionId

-- | Gets the current username from the session.
getUserFromSession :: PlepAction CommonResponse
getUserFromSession = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  sessionMap <- liftIO $ readIORef currentSessionRef
  case M.lookup sessionId sessionMap of
    Just username -> return (CommonSuccess username)
    Nothing       -> return (CommonError "User is not in session")

