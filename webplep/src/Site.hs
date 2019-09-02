{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Site
  ( runApp
  ) where

import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger          (LoggingT, runStdoutLoggingT)
import           Data.IORef
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.HVect

import           Database.Persist              hiding (get)
import qualified Database.Persist              as P
import           Database.Persist.Sqlite       hiding (get)
import           Database.Persist.TH
import           Network.Wai.Middleware.Static
import           Lucid
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid               (lucid)

import TypeDefinitions
import           Model
import Database
import Views
import Data.Maybe (fromMaybe)

runApp :: IO ()
runApp = do
  session <- newIORef M.empty
  pool <- runStdoutLoggingT $ createSqlitePool "notes.db" 5
  cfg <- defaultSpockCfg session (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock cfg app)

app :: Server ()
app = do
  get "login" $ lucid loginPage
  post "login" $ redirect "/"
  prehook authHook $ do
    middleware $ staticPolicy (addBase "static")
    get root $ do
      notes' <- runSql allNotes
      lucid $ rootHtml notes'
    post "/" $ do
      author <- param' "author"
      contents <- param' "contents"
      runSql $ insert (Note author contents)
      redirect "/"
    
-- | Authenticate the user. If 
authHook :: Web.Spock.ActionCtxT () (WebStateM SqlBackend Session ()) ()
authHook = do
  oldCtx <- getContext
  session <- readSession
  sessionMap <- liftIO $ readIORef session
  case M.lookup "user" sessionMap of   
    Nothing -> redirect "login"
    Just user -> redirect "/"
    