{-# LANGUAGE OverloadedStrings #-}

-- | Shared application monad and HTTP-error helpers used across all handler modules.
module Handler.Helpers
  ( AppM
  , runDB
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Servant (Handler)

-- | The application monad: a 'Handler' with read-only access to the database connection pool.
type AppM = ReaderT ConnectionPool Handler

-- | Run a Persistent query against the pool held in the 'AppM' environment.
runDB :: ReaderT SqlBackend IO a -> AppM a
runDB action = ask >>= liftIO . runSqlPool action
