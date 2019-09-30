module Views.Running(siteHeader, siteFooter) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

siteHeader :: Text -> Html ()
siteHeader user =
  div_ [id_ "header"] $
    if user == T.empty
      then p_ $ "Please " <> a_ [href_ "/login"] "login" <> " or " <> a_ [href_ "/register"] "register."
      else h1_ $ toHtml $ T.append (T.pack "Welcome ") user-- TODO display logout button

siteFooter :: Html ()
siteFooter = p_ "2019"