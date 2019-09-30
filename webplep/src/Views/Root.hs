module Views.Root
  ( rootPage
  ) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model
import Views.Running
import Views.ContentMain

rootPage :: Text -> [Note] -> Html ()
rootPage username notes = do
  head_ $ do
    title_ "notes"
    script_ [src_ "js/draganddrop.js"] ("" :: T.Text)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
  body_ $ do
    header
    content username notes
    p_ [id_ "todrag", draggable_ "true", ondragstart_ "drag(event)"] "drag me"
    div_ [id_ "div1", ondrop_ "drop(event)", ondragover_ "allowDrop(event)", width_ "100", height_ "70"] "here"
    footer

