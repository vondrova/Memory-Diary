{-# LANGUAGE OverloadedStrings #-}

-- | Photo storage
module Handler.Photos
  ( initPhotosDir,
    uploadPhotos,
    photosApp,
    safePhotoExtension,
    safeStoredPhotoName,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Servant (Handler, err400, errBody, throwError)
import Servant.Multipart (FileData (..), MultipartData, Tmp, files) -- for handling multipart form data (photo uploads)
import System.Directory (copyFile, createDirectoryIfMissing, getFileSize)
import System.FilePath (takeExtension, (</>))

-- | Ensure the photos directory exists before the server starts accepting requests
initPhotosDir :: FilePath -> IO ()
initPhotosDir = createDirectoryIfMissing True

-- | Serve stored photo files as a static WAI application
photosApp :: FilePath -> Application
photosApp = staticApp . defaultWebAppSettings

-- | Accept a multipart form upload and persist each file in the photos directory
-- Returns the generated filenames so the client can reference the uploaded photos
uploadPhotos :: FilePath -> MultipartData Tmp -> Handler [Text]
uploadPhotos photosPath mp = do
  validateFiles (files mp)
  liftIO $ do
    createDirectoryIfMissing True photosPath
    mapM (uncurry (saveOne photosPath)) (zip [1 :: Int ..] (files mp))

-- | Ensure uploaded filenames look like supported image files before storing them
validateFiles :: [FileData Tmp] -> Handler ()
validateFiles = mapM_ validateFile

validateFile :: FileData Tmp -> Handler ()
validateFile fd =
  case safePhotoExtension (fdFileName fd) of
    Nothing ->
      throwError err400 {errBody = "Unsupported image file type."}
    Just ext -> do
      size <- liftIO (getFileSize (fdPayload fd))
      if size > maxUploadBytes
        then throwError err400 {errBody = "Uploaded image is too large."}
        else do
          header <- liftIO (BS.take 32 <$> BS.readFile (fdPayload fd))
          if headerMatchesExtension ext header
            then pure ()
            else throwError err400 {errBody = "Uploaded file content does not match a supported image type."}

maxUploadBytes :: Integer
maxUploadBytes = 10 * 1024 * 1024

-- | Return a normalised safe image extension, if the filename has one
safePhotoExtension :: Text -> Maybe String
safePhotoExtension filename =
  let ext =
        map toLower (takeExtension (T.unpack filename))

      allowed =
        [ ".jpg",
          ".jpeg",
          ".png",
          ".webp",
          ".gif",
          ".bmp",
          ".tif",
          ".tiff",
          ".heic",
          ".heif",
          ".avif"
        ]
   in if ext `elem` allowed
        then Just ext
        else Nothing

-- | Accept only plain stored filenames, never paths or URL-like strings
safeStoredPhotoName :: Text -> Bool
safeStoredPhotoName name =
  T.strip name == name
    && not (T.null name)
    && not (T.any (`elem` ['/', '\\', '?', '#', ':']) name)
    && not (".." `T.isInfixOf` name)
    && safePhotoExtension name /= Nothing

-- | Check the file signature against the extension accepted from the filename
headerMatchesExtension :: String -> BS.ByteString -> Bool
headerMatchesExtension ext header =
  case ext of
    ".jpg" -> jpeg
    ".jpeg" -> jpeg
    ".png" -> "\137PNG\r\n\SUB\n" `BS.isPrefixOf` header
    ".gif" -> "GIF87a" `BS.isPrefixOf` header || "GIF89a" `BS.isPrefixOf` header
    ".webp" -> "RIFF" `BS.isPrefixOf` header && "WEBP" `BS.isInfixOf` BS.take 16 header
    ".bmp" -> "BM" `BS.isPrefixOf` header
    ".tif" -> tiff
    ".tiff" -> tiff
    ".heic" -> isoBaseMedia
    ".heif" -> isoBaseMedia
    ".avif" -> isoBaseMedia
    _ -> False
  where
    jpeg =
      BS.pack [0xFF, 0xD8, 0xFF] `BS.isPrefixOf` header

    tiff =
      BS.pack [0x49, 0x49, 0x2A, 0x00] `BS.isPrefixOf` header
        || BS.pack [0x4D, 0x4D, 0x00, 0x2A] `BS.isPrefixOf` header

    isoBaseMedia =
      "ftyp" `BS.isInfixOf` BS.take 16 header

-- | Generate a timestamped filename, copy the temp file to the photos directory, and return the filename
saveOne :: FilePath -> Int -> FileData Tmp -> IO Text
saveOne dir index fd = do
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S%6q" now
      ext = fromMaybe "" (safePhotoExtension (fdFileName fd))
      name = ts ++ "-" ++ show index ++ ext
      dest = dir </> name
  copyFile (fdPayload fd) dest
  pure (T.pack name)
