module Views
  ( loginPage
  , registerPage
  , rootPage
  ) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

loginPage :: Html ()
loginPage =
  form_ [method_ "post"] $ do
        label_ $ do
          "Username: "
          input_ [name_ "username"]
        label_ $ do
          "Password: "
          input_ [name_ "password"]
        input_ [type_ "submit", value_ "Log In"]

registerPage :: Html ()
registerPage =
  form_ [method_ "post"] $ do
        label_ $ do
          "Username: "
          input_ [name_ "username"]
        label_ $ do
          "Password: "
          input_ [name_ "password"]
        input_ [type_ "submit", value_ "Log In"]


rootPage :: Text -> [Note] -> Html ()
rootPage username notes = do
  head_ $ do
    title_ "notes"
    script_ [src_ "js/draganddrop.js"] ("" :: T.Text)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
  body_ $ do
    h1_ $ toHtml username
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
