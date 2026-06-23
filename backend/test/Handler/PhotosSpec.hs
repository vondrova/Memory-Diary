{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the filename safety checks in 'Handler.Photos'
--
--   * safePhotoExtension   — accepts .jpg/.jpeg/.png/.gif/.webp (any case);
--                              rejects missing, unknown, or double extensions
--   * safeStoredPhotoName  — accepts filenames that look like stored uploads
--                              (UUID + safe extension); rejects directory traversal,
--                              URL-like strings, and other unsafe patterns

module Handler.PhotosSpec (spec) where

import Handler.Photos (safePhotoExtension, safeStoredPhotoName)
import Test.Hspec

spec :: Spec
spec =
  describe "safePhotoExtension" $ do
    it "accepts common image file extensions case-insensitively" $ do
      safePhotoExtension "photo.JPG" `shouldBe` Just ".jpg"
      safePhotoExtension "scan.tiff" `shouldBe` Just ".tiff"
      safePhotoExtension "phone.HEIC" `shouldBe` Just ".heic"
      safePhotoExtension "modern.avif" `shouldBe` Just ".avif"

    it "rejects missing or unsafe file extensions" $ do
      safePhotoExtension "photo" `shouldBe` Nothing
      safePhotoExtension "script.svg" `shouldBe` Nothing
      safePhotoExtension "program.exe" `shouldBe` Nothing

    it "accepts stored photo filenames but rejects paths and URL-like values" $ do
      safeStoredPhotoName "20260615120000000000-1.jpg" `shouldBe` True
      safeStoredPhotoName "../secret.jpg" `shouldBe` False
      safeStoredPhotoName "nested/photo.jpg" `shouldBe` False
      safeStoredPhotoName "photo.jpg?raw=1" `shouldBe` False
