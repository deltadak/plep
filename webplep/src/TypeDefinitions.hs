module TypeDefinitions
  ( Server
  , Session
  ) where

import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock               (SpockM)
import Data.IORef (IORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

-- | SpockM (database connection) (session type) (state type) (monadic return value's type)
type Server a = SpockM SqlBackend Session () a

type Session = IORef (M.Map T.Text T.Text)