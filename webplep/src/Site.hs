{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

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

runApp :: IO ()
runApp = do
  pool <- runStdoutLoggingT $ createSqlitePool "notes.db" 5
  cfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock cfg app)

app :: Server ()
app = do
  middleware $ staticPolicy (addBase "static")
  get root $ do
    notes' <- runSql allNotes
    lucid $ rootHtml notes'
  post root $ do
    author <- param' "author"
    contents <- param' "contents"
    runSql $ insert (Note author contents)
    redirect "/"
  get ("hello" <//> var) $ \x -> lucid $ helloHtml x