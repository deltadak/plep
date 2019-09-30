{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Site
  ( runApp
  ) where

import           Control.Monad                 (forM_, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger          (LoggingT, runStdoutLoggingT)
import           Data.HVect hiding (length)
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
import           Views.Root
import Views.LoginRegisterForms
import Views.ContentMain
import Views.Running
import Views.Welcome

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
  get "welcome" $ lucid $ rootPage (siteHeader T.empty) welcome siteFooter
  get "login" $ lucid $ rootPage (siteHeader T.empty) loginForm siteFooter
  post "login" $ do
    username <- param' "username"
    password <- param' "password"
    loginAction (User username password)
    redirect "/"
  get "register" $ lucid $ rootPage (siteHeader T.empty) registerForm siteFooter
  post "register" $ do
    username <- param' "username"
    password <- param' "password"
    response <- registerAction (User username password)
    case response of
      CommonSuccess _ -> redirect "login"
      CommonError _ -> redirect "register"
  prehook authHook $ do
    get root $ do
      notes' <- runSql allNotes
      users <- runSql allUsers
      response <- getUserFromSession
      case response of
        CommonSuccess username -> lucid $ rootPage (siteHeader username) (content username notes') siteFooter
        CommonError error -> redirect "welcome"
    post root $ do
      contents <- param' "note"
      currentUser <- getUserFromSession
      case currentUser of
        CommonSuccess username -> do runSql $ insert (Note username contents)
                                     redirect "/"
        CommonError error -> redirect "welcome"                                    
      

-- | Check if there is a user set in the session.
authHook :: PlepAction ()
authHook = do
  sessionId <- getSessionId
  sessionRef <- readSession
  sessionMap <- liftIO $ readIORef sessionRef
  case M.lookup sessionId sessionMap of
    Nothing   -> redirect "welcome"
    Just user -> return ()

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


-- | Validate a user by checking if there is exactly one user with this name and password in the database.
validateUserAction :: User -> PlepAction Bool
validateUserAction user = do
  foundUsers <- runSql $ getUserByCredentials user
  case length foundUsers of
    1 -> return True
    _ -> return False

-- | Gets the current username from the session.
getUserFromSession :: PlepAction CommonResponse
getUserFromSession = do
  sessionId <- getSessionId
  currentSessionRef <- readSession
  sessionMap <- liftIO $ readIORef currentSessionRef
  case M.lookup sessionId sessionMap of
    Just username -> return (CommonSuccess username)
    Nothing -> return (CommonError "User is not in session")
    