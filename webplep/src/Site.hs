{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Site
  ( runApp
  ) where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Logger            (LoggingT, runStdoutLoggingT)
import           Data.HVect                      hiding (length)
import           Data.IORef
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Semigroup                  ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Database.Persist                hiding (get)
import qualified Database.Persist                as P
import           Database.Persist.Sqlite         hiding (get)
import           Database.Persist.TH
import           Lucid
import           Network.Wai.Middleware.Static
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid                 (lucid)

import           Control.Monad.Cont              (lift)
import           Data.Maybe                      (fromMaybe)
import           Database
import           GHC.IO                          (evaluate)
import           Model

import           Actions.LoginRegisterValidation
import           Actions.UserAuthentication
import           Actions.UserManagement
import           TypeDefinitions
import           Views.ContentMain
import           Views.LoginRegisterForms
import           Views.Root
import           Views.Running
import           Views.Welcome

runApp :: IO ()
runApp = do
  session <- newIORef M.empty
  pool <- runStdoutLoggingT $ createSqlitePool "notes.db" 5
  cfg <- defaultSpockCfg session (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock cfg app)

-- | Handles the routing of urls, and runs the app.
app :: Server ()
app = do
  middleware $ staticPolicy (addBase "static")
  get "welcome" $ lucid $ rootPage (siteHeader T.empty) welcome siteFooter
  get "login" $ lucid $ rootPage (siteHeader T.empty) loginForm siteFooter
  post "login" $ do
    username <- param' "username"
    password <- param' "password"
    if not $ T.null password
      then do
        loginAction (User username password)
        redirect "/"
      else redirect "login"
  get "register" $ lucid $ rootPage (siteHeader T.empty) registerForm siteFooter
  post "register" $ do
    username <- param' "username"
    password <- param' "password"
    -- Check if the entered fields are valid.
    validateUser <- validateRegister username password
    if validateUser
      then do
        response <- registerAction (User username password)
        case response of
          CommonSuccess _ -> redirect "login"
          CommonError _   -> redirect "register"
      else redirect "register"
  prehook authHook $ do
    get root $ do
      response <- getUserFromSession
      case response of
        CommonSuccess username -> do
          notes' <- runSql $ allNotesFromUser username
          lucid $ rootPage (siteHeader username) (content username notes') siteFooter
        CommonError error -> redirect "welcome"
    post root $ do
      contents <- param' "note"
      currentUser <- getUserFromSession
      case currentUser of
        CommonSuccess username -> do
          runSql $ insert (Note username contents)
          redirect "/"
        CommonError error -> redirect "welcome"
    get "logout" $ do
      logoutAction
      redirect "welcome" --  Check if there is a user set in the session.