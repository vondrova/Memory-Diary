{-# LANGUAGE OverloadedStrings #-}

-- | Photo upload and static-serving handler.
module Handler.Photos
  ( uploadPhotos
  , photosApp
  ) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Servant.Multipart (MultipartData, Tmp, FileData, files, fdFileName, fdPayload)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeExtension, (</>))
import Servant (Handler)

-- | Serve stored photo files as a static WAI application.
photosApp :: FilePath -> Application
photosApp = staticApp . defaultWebAppSettings

-- | Accept a multipart form upload and persist each file to the photos directory.
uploadPhotos :: FilePath -> MultipartData Tmp -> Handler [String]
uploadPhotos photosPath multipartData = liftIO $ do
  createDirectoryIfMissing True photosPath
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
  zipWithM (saveFile ts photosPath) [1 :: Int ..] (files multipartData)

saveFile :: String -> FilePath -> Int -> FileData Tmp -> IO String
saveFile ts dir idx fd = do
  let ext  = takeExtension (T.unpack (fdFileName fd))
      name = ts ++ show idx ++ ext
      dest = dir </> name
  copyFile (fdPayload fd) dest
  return name
