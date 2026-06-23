{-# LANGUAGE OverloadedStrings #-}

-- | JSON serialisation tests for all public DTOs defined in 'Types'
--
--   Covered types: 'MemoryInput', 'ImportantDayInput', 'NoteInput', 'PlanInput',
--   'RenameInput', 'DeleteCategoryInput', 'RelationshipInput', 'ProfileInput',
--   'DiaryInput'
module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isInfixOf)
import Test.Hspec
import TestHelpers (atDay)
import Types

spec :: Spec
spec = do
  describe "MemoryInput JSON" $
    it "round-trips the public memory payload" $
      decodeEncoded memoryInput `shouldBe` Just memoryInput

  describe "ImportantDayInput JSON" $
    it "round-trips important day payloads" $
      decodeEncoded importantDayInput `shouldBe` Just importantDayInput

  describe "NoteInput JSON" $
    it "round-trips note payloads" $
      decodeEncoded noteInput `shouldBe` Just noteInput

  describe "PlanInput JSON" $
    it "round-trips plan payloads" $
      decodeEncoded planInput `shouldBe` Just planInput

  describe "bulk operation JSON" $ do
    it "round-trips rename payloads" $
      decodeEncoded (RenameInput "new-name") `shouldBe` Just (RenameInput "new-name")

    it "round-trips category delete scope payloads" $
      decodeEncoded (DeleteCategoryInput (Just "pending")) `shouldBe` Just (DeleteCategoryInput (Just "pending"))

    it "allows category delete payloads without a scope" $
      A.decode "{}" `shouldBe` Just (DeleteCategoryInput Nothing)

  describe "RelationshipInput JSON" $
    it "round-trips relationship payloads" $
      decodeEncoded relationshipInput `shouldBe` Just relationshipInput

  describe "ProfileInput JSON" $ do
    it "round-trips profile payloads" $
      decodeEncoded profileInput `shouldBe` Just profileInput

    it "does not duplicate the URL-owned profile side in the payload" $
      encode profileInput `shouldNotContainJsonField` "\"side\""

  describe "row encoders" $
    it "keep frontend-facing field names stable" $ do
      encode (MemoryRow 7 "Trip" (atDay 2026 1 1) (atDay 2026 1 2) Nothing (Just "Prague") [] [])
        `shouldContainJsonField` "\"timeFrom\""
      encode (ImportantDayRow (Just 1) "Birthday" (atDay 2026 6 18) Nothing "manual" Nothing (atDay 2027 6 18) 362 6 18)
        `shouldContainJsonField` "\"title\""
      encode (NoteRow 1 "shared" "Allergy" "Peanuts")
        `shouldContainJsonField` "\"owner\""
      encode (PlanRow 1 "Trips" "Brno" Nothing False)
        `shouldContainJsonField` "\"done\""
      encode (RelationshipRow (Just (atDay 2026 1 1)) "heart-red" (Just 10) Nothing Nothing Nothing Nothing Nothing Nothing)
        `shouldContainJsonField` "\"daysTogether\""
      encode (ProfileRow "left" "A" "figure" Nothing "figure-red" "none" "neutral" Nothing)
        `shouldContainJsonField` "\"displayMode\""
      encode (TrashRow 1 "memory" "Trip" "2026-01-01T00:00:00Z")
        `shouldContainJsonField` "\"deletedAt\""

decodeEncoded :: (A.FromJSON a, A.ToJSON a) => a -> Maybe a
decodeEncoded =
  decode . encode

shouldContainJsonField :: BSL.ByteString -> BSL.ByteString -> Expectation
shouldContainJsonField payload field =
  payload `shouldSatisfy` \value -> BSL.unpack field `isInfixOf` BSL.unpack value

shouldNotContainJsonField :: BSL.ByteString -> BSL.ByteString -> Expectation
shouldNotContainJsonField payload field =
  payload `shouldSatisfy` \value -> not (BSL.unpack field `isInfixOf` BSL.unpack value)

memoryInput :: MemoryInput
memoryInput =
  MemoryInput "Trip" (atDay 2026 1 1) (atDay 2026 1 2) (Just "Nice") (Just "Prague") ["trip"] []

importantDayInput :: ImportantDayInput
importantDayInput =
  ImportantDayInput "Birthday" (atDay 2026 6 18) Nothing

noteInput :: NoteInput
noteInput =
  NoteInput "shared" "Allergy" "Peanuts"

planInput :: PlanInput
planInput =
  PlanInput "Trips" "Visit Brno" Nothing False

relationshipInput :: RelationshipInput
relationshipInput =
  RelationshipInput (Just (atDay 2026 1 1)) "heart-red"

profileInput :: ProfileInput
profileInput =
  ProfileInput "A" "figure" Nothing "figure-red" "none" "neutral" (Just (atDay 2000 1 1))
