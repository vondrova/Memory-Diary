{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handler for the api stats endpoint, returning aggregate statistics about the stored memories
module Handler.Stats
  ( statsServer,
  )
where

import qualified Api
import Handler.Helpers (AppM, runDb)
import Database.Persist (selectList)
import Domain.Aggregation (computeStats)
import Handler.Memories (memoryRowFromEntity)
import Models (Stats)
import Servant (ServerT)

statsServer :: ServerT Api.StatsApi AppM
statsServer = getStats

-- | Compute and return aggregate statistics over all stored memories
getStats :: AppM Stats
getStats = do
  memories <- runDb (selectList [] [])
  rows <- mapM memoryRowFromEntity memories
  pure (computeStats rows)
