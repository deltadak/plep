module Views.Running(header, footer) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

header :: Html ()
header = h1_ "Header"

footer :: Html ()
footer = p_ "2019"