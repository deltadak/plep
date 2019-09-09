module TypeDefinitions
  ( Server
  , PlepSession
  , CommonResponse (..)
  , PlepAction (..)
  ) where

import           Data.IORef              (IORef)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock

-- | SpockM (database connection) (session type) (state type) (monadic return value's type)
type Server a = SpockM SqlBackend PlepSession () a

type PlepSession = IORef (M.Map T.Text T.Text)

data CommonResponse
  = CommonError T.Text
  | CommonSuccess T.Text
  deriving (Show, Eq)

type PlepAction a = SpockActionCtx () SqlBackend PlepSession () a
