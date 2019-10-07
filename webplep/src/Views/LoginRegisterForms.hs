module Views.LoginRegisterForms(loginForm, registerForm) where

import           Data.Text     (Text)
import qualified Data.Text     as T

import           Lucid

import           Model

-- | 
loginForm :: Html ()
loginForm =
  form_ [method_ "post"] $ do
        label_ $ do
          "Username: "
          input_ [name_ "username"]
        label_ $ do
          "Password: "
          input_ [name_ "password"]
        input_ [type_ "submit", value_ "Log In"]

registerForm :: Html ()
registerForm =
  form_ [method_ "post"] $ do
        label_ $ do
          "Username: "
          input_ [name_ "username"]
        label_ $ do
          "Password: "
          input_ [name_ "password"]
        input_ [type_ "submit", value_ "Register"]