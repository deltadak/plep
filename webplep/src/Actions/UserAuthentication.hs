module Actions.UserAuthentication
  ( authHook
  , validateUserAction
  ) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.IORef
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Database.Persist.Sqlite hiding (get)
import           Web.Spock

import           Database
import           Model
import           TypeDefinitions         (PlepAction)

-- | Check if there is a user set in the session, if not: redirect to the welcome page.
-- This is to protect views that should only be accessible to logged in users.
authHook :: PlepAction ()
authHook = do
  sessionId <- getSessionId
  sessionRef <- readSession
  sessionMap <- liftIO $ readIORef sessionRef
  case M.lookup sessionId sessionMap of
    Nothing   -> redirect "welcome"
    Just user -> return ()

-- | Validate a user by checking if there is exactly one user with this name and password in the database.
validateUserAction :: User -> PlepAction Bool
validateUserAction user = do
  foundUsers <- runSql $ getUserByCredentials user
  case length foundUsers of
    1 -> return True
    _ -> return False