{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for bulk tag, location and plan-category operation
module Handler.Tags
  ( tagServer,
    locationServer,
    planCategoryServer,
    listTags,
    createTag,
    renameTag,
    deleteTag,
    listLocations,
    createLocation,
    renameLocation,
    deleteLocation,
    listPlanCategories,
    createPlanCategory,
    renamePlanCategory,
    deletePlanCategory,
  )
where

import qualified Api

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Domain.Validation (validateRenameInputFields)
import Handler.Helpers (AppM, runDb, validateInput)
import Models
import Servant
import Types

tagServer :: ServerT Api.TagApi AppM
tagServer = listTags :<|> createTag :<|> renameTag :<|> deleteTag

locationServer :: ServerT Api.LocationApi AppM
locationServer = listLocations :<|> createLocation :<|> renameLocation :<|> deleteLocation

planCategoryServer :: ServerT Api.PlanCategoryApi AppM
planCategoryServer = listPlanCategories :<|> createPlanCategory :<|> renamePlanCategory :<|> deletePlanCategory

-- TAGS ----------------------------------------

listTags :: AppM [Text]
listTags = map (tagCatalogDbName . entityVal) <$> runDb (selectList [] [Asc TagCatalogDbName])

createTag :: CatalogInput -> AppM NoContent
createTag input = do
  name <- validateCatalogInput input
  _ <- runDb (insertUnique (TagCatalogDb name))
  pure NoContent

-- | Replace every occurrence of oldName tag with the given new name across all memories.
renameTag :: Text -> RenameInput -> AppM NoContent
renameTag oldName input = do
  validateInput (validateRenameInputFields input)
  let oldNameN = T.strip oldName
      newName = T.strip (riNewName input)
  if oldNameN == newName
    then pure ()
    else runDb $ do
      _ <- insertUnique (TagCatalogDb newName)
      deleteBy (UniqueTagCatalog oldNameN)
      oldRows <- selectList [MemoryTagDbTag ==. oldNameN] []
      newRows <- selectList [MemoryTagDbTag ==. newName] []
      let memoryIdsWithNewTag =
            Set.fromList (map (memoryTagDbMemoryId . entityVal) newRows)
          conflictingOldIds =
            filter
              (`Set.member` memoryIdsWithNewTag)
              (map (memoryTagDbMemoryId . entityVal) oldRows)
      if null conflictingOldIds
        then pure ()
        else
          deleteWhere -- if there are any memories that already have the new tag, we must delete the old tag from those memories instead of renaming, to avoid duplicate (memory, tag) pairs which would violate the unique constraint
            [ MemoryTagDbTag ==. oldNameN,
              MemoryTagDbMemoryId <-. conflictingOldIds
            ]
      updateWhere [MemoryTagDbTag ==. oldNameN] [MemoryTagDbTag =. newName]
  pure NoContent

-- | Remove every occurrence of name tag across all memories
deleteTag :: Text -> AppM NoContent
deleteTag name = do
  runDb $ do
    deleteWhere [MemoryTagDbTag ==. name]
    deleteBy (UniqueTagCatalog name)
  pure NoContent

-- LOCATIONS ----------------------------------------

listLocations :: AppM [Text]
listLocations = map (locationCatalogDbName . entityVal) <$> runDb (selectList [] [Asc LocationCatalogDbName])

createLocation :: CatalogInput -> AppM NoContent
createLocation input = do
  name <- validateCatalogInput input
  _ <- runDb (insertUnique (LocationCatalogDb name))
  pure NoContent

-- | Replace every oldName location with the given new name across all memories
renameLocation :: Text -> RenameInput -> AppM NoContent
renameLocation oldName input = do
  validateInput (validateRenameInputFields input)
  let oldNameN = T.strip oldName
      newName = T.strip (riNewName input)
  if oldNameN == newName
    then pure ()
    else runDb $ do
      _ <- insertUnique (LocationCatalogDb newName)
      deleteBy (UniqueLocationCatalog oldNameN)
      updateWhere [MemoryDbLocation ==. Just oldNameN] [MemoryDbLocation =. Just newName]
  pure NoContent

-- | Clear the location field from every memory that has the given location
deleteLocation :: Text -> AppM NoContent
deleteLocation name = do
  runDb $ do
    updateWhere [MemoryDbLocation ==. Just name] [MemoryDbLocation =. Nothing]
    deleteBy (UniqueLocationCatalog name)
  pure NoContent

-- PLAN CATEGORIES ----------------------------------------

listPlanCategories :: AppM [Text]
listPlanCategories = map (planCategoryDbName . entityVal) <$> runDb (selectList [] [Asc PlanCategoryDbName])

createPlanCategory :: CatalogInput -> AppM NoContent
createPlanCategory input = do
  name <- validateCatalogInput input
  _ <- runDb (insertUnique (PlanCategoryDb name))
  pure NoContent

-- | Rename a plan category
renamePlanCategory :: Text -> RenameInput -> AppM NoContent
renamePlanCategory oldName input = do
  validateInput (validateRenameInputFields input)
  let oldNameN = T.strip oldName
      newName = T.strip (riNewName input)
  if oldNameN == newName
    then pure ()
    else runDb $ do
      _ <- insertUnique (PlanCategoryDb newName)
      deleteBy (UniquePlanCategory oldNameN)
      updateWhere [CouplePlanDbCategory ==. oldNameN] [CouplePlanDbCategory =. newName]
  pure NoContent

-- | Delete all plans in a category
deletePlanCategory :: Text -> DeleteCategoryInput -> AppM NoContent
deletePlanCategory name input = do
  let filters = (CouplePlanDbCategory ==. name) : scopeFilter (fromMaybe "all" (dciScope input))
  runDb $ do
    deleteWhere filters
    remaining <- count [CouplePlanDbCategory ==. name]
    if remaining == 0 then deleteBy (UniquePlanCategory name) else pure ()
  pure NoContent
  where
    scopeFilter "pending" = [CouplePlanDbDone ==. False]
    scopeFilter "done" = [CouplePlanDbDone ==. True]
    scopeFilter _ = []

-- | Validate a 'CatalogInput' and return the cleaned name
validateCatalogInput :: CatalogInput -> AppM Text
validateCatalogInput input = do
  validateInput (validateRenameInputFields (RenameInput (ciName input)))
  pure (T.strip (ciName input))
