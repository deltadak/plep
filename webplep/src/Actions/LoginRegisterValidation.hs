module Actions.LoginRegisterValidation
  ( validateRegisterUsername
  , validateRegisterPassword
  , validateRegister
  ) where

import           Data.Text               (Text)
import qualified Data.Text               as T
import           Database.Persist.Sqlite hiding (get)

import           Database
import           Model
import           TypeDefinitions         (PlepAction)

-- | A username cannot be empty, and cannot be already taken.
validateRegisterUsername :: Text -> PlepAction Bool
validateRegisterUsername username = do
  response <- runSql $ getUserByName username
  case response of
    [] -> return (not (T.null username)) -- The username is still available, so return False only if it is the empty string.
    _  -> return False -- The username is taken, so return False.

-- | Test that the password is not empty.
validateRegisterPassword :: Text -> Bool
validateRegisterPassword password = not $ T.null password

-- | Test if the entered fields are valid to register, i.e., if the given username is valid by the rules of 'validateRegisterUsername'
-- and if the password is valid by the rules of 'validateRegisterPassword'.
validateRegister :: Text -> Text -> PlepAction Bool
validateRegister name password = do
  validateUsername <- validateRegisterUsername name
  return (validateUsername && validateRegisterPassword password)
