module FormValidation.LoginRegister(validateRegisterUsername, validateRegisterPassword) where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           Database.Persist.Sqlite       hiding (get)

import Model
import Database
import TypeDefinitions (PlepAction)

-- | A username cannot be empty, and cannot be already taken.
validateRegisterUsername :: Text -> PlepAction Bool
validateRegisterUsername username = do 
  response <- runSql $ getUserByName username
  case response of
   -- The username is still available, so return False only if it is the empty string.
   [] -> return (not (T.null username))
   -- The username is taken, so return False.
   _ -> return False
   
   
validateRegisterPassword :: Text -> Bool 
validateRegisterPassword password = not $ T.null password
