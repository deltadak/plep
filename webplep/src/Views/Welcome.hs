module Views.Welcome(welcome) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

welcome :: Html ()
welcome = p_ "Welcome to Plep!"