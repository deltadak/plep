module Views.ContentMain(content) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

content :: Text -> [Note] -> Html ()
content username notes = do 
  h1_ $ toHtml username
  h2_ "Notes"
  ul_ $
    forM_ notes $ \note ->
      li_ $ do
        toHtml (noteAuthor note)
        ": "
        toHtml (noteContents note)
  h2_ "New Note"
  form_ [method_ "post"] $ do
    label_ $ do
      "Note: "
      textarea_ [name_ "note"] ""
    input_ [type_ "submit", value_ "Add Note"]