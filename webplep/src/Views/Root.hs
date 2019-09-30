module Views.Root
  ( rootPage
  ) where

import           Control.Monad     (forM_)
import           Data.Text         (Text)
import qualified Data.Text         as T

import           Lucid

import           Model

-- | The root page combines the content with the header, footer, css, and javascript.
rootPage ::
     Html () -- | The header of the page.
  -> Html () -- | The content of the page.
  -> Html () -- | The footer of the page.
  -> Html () -- | Complete page.
rootPage header content footer = do
  head_ $ do
    title_ "notes"
    script_ [src_ "js/draganddrop.js"] ("" :: T.Text)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
  body_ $ do
    header
    content
    footer
--    p_ [id_ "todrag", draggable_ "true", ondragstart_ "drag(event)"] "drag me"
--    div_ [id_ "div1", ondrop_ "drop(event)", ondragover_ "allowDrop(event)", width_ "100", height_ "70"] "here"
