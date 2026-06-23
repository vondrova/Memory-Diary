{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/plans endpoints.
module Handler.Plans
  ( planServer
  ) where

import Api (PlanAPI)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

planServer :: ServerT PlanAPI AppM
planServer = getPlans :<|> createPlan :<|> updatePlan :<|> deletePlan

-- | Return all plans sorted by category.
getPlans :: AppM [CouplePlan]
getPlans = map toCouplePlan <$> runDB (selectList [] [Asc CouplePlanDbCategory])

-- | Create a new plan.
createPlan :: CouplePlanInput -> AppM CouplePlan
createPlan input = do
  let db = CouplePlanDb
        { couplePlanDbCategory = T.pack (couplePlanInputCategory input)
        , couplePlanDbTitle    = T.pack (couplePlanInputTitle input)
        , couplePlanDbDetail   = T.pack <$> couplePlanInputDetail input
        , couplePlanDbDone     = couplePlanInputDone input
        }
  key <- runDB $ insert db
  return (toCouplePlan (Entity key db))

-- | Replace all fields of an existing plan.
updatePlan :: Int -> CouplePlanInput -> AppM CouplePlan
updatePlan planId input = do
  let key = toSqlKey (fromIntegral planId) :: Key CouplePlanDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = CouplePlanDb
            { couplePlanDbCategory = T.pack (couplePlanInputCategory input)
            , couplePlanDbTitle    = T.pack (couplePlanInputTitle input)
            , couplePlanDbDetail   = T.pack <$> couplePlanInputDetail input
            , couplePlanDbDone     = couplePlanInputDone input
            }
      runDB $ replace key db
      return (toCouplePlan (Entity key db))

-- | Delete a plan by id.
deletePlan :: Int -> AppM NoContent
deletePlan planId = do
  let key = toSqlKey (fromIntegral planId) :: Key CouplePlanDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent
