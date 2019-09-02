module Views (helloHtml, rootHtml) where

import Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad                 (forM_)

import Lucid

import Model

helloHtml :: Text -> Html ()
helloHtml name =
  h1_ $ do
    "hello "
    toHtml name

rootHtml :: [Note] -> Html ()
rootHtml notes = do
  head_ $ do
    title_ "notes"
    script_ [src_ "js/draganddrop.js"] ("" :: T.Text)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
  body_ $ do
    h1_ "hi!"
    ul_ $
      forM_ notes $ \note ->
        li_ $ do
          toHtml (noteAuthor note)
          ": "
          toHtml (noteContents note)
    h2_ "New Note"
    form_ [method_ "post"] $ do
      label_ $ do
        "Author: "
        input_ [name_ "author"]
      label_ $ do
        "Contents: "
        textarea_ [name_ "contents"] ""
      input_ [type_ "submit", value_ "Add Note"]
    p_ [id_ "todrag", draggable_ "true", ondragstart_ "drag(event)"] "drag me"
    div_ [id_ "div1", ondrop_ "drop(event)", ondragover_ "allowDrop(event)", width_ "100", height_ "70"] "here"