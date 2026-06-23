{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api plans endpoints
module Handler.Plans
  ( planServer,
    listPlans,
    createPlan,
    getPlan,
    updatePlan,
    deletePlan,
  )
where

import qualified Api
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Domain.Validation (cleanMaybeText, validatePlanInputFields)
import Handler.Helpers
import Models
import Servant
import Types

planServer :: ServerT Api.PlanApi AppM
planServer = listPlans :<|> createPlan :<|> updatePlan :<|> deletePlan

-- | Return all plans sorted by category, completion status, then title
listPlans :: AppM [PlanRow]
listPlans = do
  plans <- runDb (selectList [] [Asc CouplePlanDbCategory, Asc CouplePlanDbDone, Asc CouplePlanDbTitle])
  pure (map planRowFromEntity plans)

-- | Create a new plan and return the persisted row
createPlan :: PlanInput -> AppM PlanRow
createPlan input = do
  validatePlan input
  planId <- runDb $ do
    let plan = planDbFromInput input
    _ <- insertUnique (PlanCategoryDb (couplePlanDbCategory plan))
    insert plan
  getPlan (toIntKey planId)

-- | Replace all fields of an existing plan
updatePlan :: Int -> PlanInput -> AppM NoContent
updatePlan rawId input = do
  validatePlan input
  let planId = fromIntKey rawId :: CouplePlanDbId
  existing <- runDb (get planId)
  case existing of
    Nothing -> notFound
    Just _ -> do
      runDb $ do
        let plan = planDbFromInput input
        _ <- insertUnique (PlanCategoryDb (couplePlanDbCategory plan))
        replace planId plan
      pure NoContent

-- | Soft-delete a plan by moving it to the trash table
--   The trash insert and the row delete run in one transaction
deletePlan :: Int -> AppM NoContent
deletePlan rawId = do
  row <- getPlan rawId
  runDb $ do
    now <- liftIO getCurrentTime
    insert_ (TrashItemDb "plan" (encodeToText (PlanInput (prCategory row) (prTitle row) (prDetail row) (prDone row))) now)
    delete (fromIntKey rawId :: CouplePlanDbId)
  pure NoContent

-- | Fetch a single plan by numeric id
getPlan :: Int -> AppM PlanRow
getPlan rawId = do
  mPlan <- runDb (getEntity (fromIntKey rawId :: CouplePlanDbId))
  case mPlan of
    Nothing -> notFound
    Just plan -> pure (planRowFromEntity plan)

-- | Validate a 'PlanInput'
validatePlan :: PlanInput -> AppM ()
validatePlan = validateInput . validatePlanInputFields

-- | Map a 'PlanInput' to a database record, normalising a blank category to "other"
planDbFromInput :: PlanInput -> CouplePlanDb
planDbFromInput input =
  let category = T.strip (piCategory input)
   in CouplePlanDb
        (if T.null category then "Other" else category)
        (T.strip (piTitle input))
        (cleanMaybeText (piDetail input))
        (piDone input)

-- | Convert a Persistent entity to the JSON row type
planRowFromEntity :: Entity CouplePlanDb -> PlanRow
planRowFromEntity (Entity planId plan) =
  PlanRow
    (toIntKey planId)
    (couplePlanDbCategory plan)
    (couplePlanDbTitle plan)
    (couplePlanDbDetail plan)
    (couplePlanDbDone plan)
