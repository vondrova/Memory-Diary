-- | HTTP handler for the /api/stats endpoint.
module Handler.Stats
  ( getStats
  ) where

import Api (StatsAPI)
import Database.Persist (selectList)
import Domain.Aggregation (computeStats)
import Handler.Helpers (AppM, runDB)
import Models (toMemory)
import Servant (ServerT)
import Types (Stats)

-- | Compute and return aggregate statistics over all stored memories.
getStats :: ServerT StatsAPI AppM
getStats = do
  entities <- runDB $ selectList [] []
  return $ computeStats (map toMemory entities)
