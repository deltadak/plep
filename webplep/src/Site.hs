{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Site
  ( runApp
  ) where

import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger          (LoggingT, runStdoutLoggingT)
import           Data.HVect
import           Data.IORef
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Database.Persist              hiding (get)
import qualified Database.Persist              as P
import           Database.Persist.Sqlite       hiding (get)
import           Database.Persist.TH
import           Lucid
import           Network.Wai.Middleware.Static
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid               (lucid)

import           Control.Monad.Cont            (lift)
import           Data.Maybe                    (fromMaybe)
import           Database
import           GHC.IO                        (evaluate)
import           Model
import           TypeDefinitions
import           Views

runApp :: IO ()
runApp = do
  session <- newIORef M.empty
  pool <- runStdoutLoggingT $ createSqlitePool "notes.db" 5
  cfg <- defaultSpockCfg session (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock cfg app)

app :: Server ()
app = do
  middleware $ staticPolicy (addBase "static")
  get "login" $ lucid loginPage
  post "login" $ do
    username <- param' "username"
    loginAction (User username)
    redirect "/"
  get "register" $ lucid registerPage
  post "register" $ do
    username <- param' "username"
    response <- registerAction (User username)
    case response of
      CommonSuccess _ -> redirect "login"
      CommonError _   -> redirect "register"
  prehook authHook $ do
    get root $ do
      notes' <- runSql allNotes
      response <- getUserFromSession
      case response of
        CommonSuccess username -> lucid $ rootPage username notes'
        CommonError error -> redirect "login"
 
    post root $ do
      author <- param' "author"
      contents <- param' "contents"
      runSql $ insert (Note author contents)
      redirect "/"

-- | Check if there is a user set in the session.
-- TODO check if user in database. If not, display login page with "user not known"
authHook :: PlepAction ()
authHook = do
  sessionId <- getSessionId
  sessionRef <- readSession
  sessionMap <- liftIO $ readIORef sessionRef
  case M.lookup sessionId sessionMap of
    Nothing   -> redirect "login"
    Just user -> return ()

-- | When registering, add a user to the database.
registerAction :: User -> PlepAction CommonResponse
registerAction user = runSql $ addUser user

-- | When logging in, put the user in the current session.
loginAction :: User -> PlepAction ()
loginAction user = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  liftIO $ modifyIORef' currentSessionRef $
    M.insert sessionId (userUsername user)
    
getUserFromSession :: PlepAction CommonResponse
getUserFromSession = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  sessionMap <- liftIO $ readIORef currentSessionRef
  case M.lookup sessionId sessionMap of
    Just username -> return (CommonSuccess username)
    Nothing -> return (CommonError "User does not exist")
    