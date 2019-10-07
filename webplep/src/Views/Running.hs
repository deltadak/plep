module Views.Running(siteHeader, siteFooter) where

import           Control.Monad (forM_)
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

-- | The header of the site. Shows a welcome message when given a user, otherwise asks you to log in or register.
siteHeader :: Text     -- ^ Username of a user. If empty this means there is no user.
           -> Html ()  -- ^ Resulting HTML.
siteHeader user =
  div_ [id_ "header"] $
    if user == T.empty
      then p_ $ "Please " <> a_ [href_ "/login"] "login" <> " or " <> a_ [href_ "/register"] "register."
      else h1_ $ toHtml (T.append (T.pack "Welcome ") user ) <> a_ [href_ "/logout"] "logout"

-- | The footer of the site.
siteFooter :: Html ()
siteFooter = p_ "2019"