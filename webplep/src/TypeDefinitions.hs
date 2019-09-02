module TypeDefinitions
  ( Server
  ) where

import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock               (SpockM)

-- | SpockM (database connection) (session type) (state type) (monadic return value's type)
type Server a = SpockM SqlBackend () () a
