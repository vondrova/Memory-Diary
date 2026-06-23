{-# LANGUAGE OverloadedStrings #-}

-- | Geocoding proxy handler — forwards queries to Nominatim and returns results.
module Handler.Geocode
  ( geocodeHandler
  ) where

import Api (GeoAPI)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Data.Text as T
import Handler.Helpers (AppM)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader)
import Servant (ServerT)
import Types (GeoResult)

geocodeHandler :: ServerT GeoAPI AppM
geocodeHandler = geocode

-- | Forward a free-text query to Nominatim and return place suggestions.
geocode :: Maybe Text -> AppM [GeoResult]
geocode Nothing  = return []
geocode (Just q) = liftIO $ do
  let encoded = concatMap encodeChar (T.unpack q)
      url     = "https://nominatim.openstreetmap.org/search?q=" ++ encoded ++ "&format=json&limit=5"
  req <- parseRequest url
  let req' = setRequestHeader "User-Agent" ["MemoryDiary/1.0"] req
  resp <- httpLBS req'
  case decode (getResponseBody resp) of
    Nothing  -> return []
    Just res -> return res
  where
    encodeChar ' ' = "%20"
    encodeChar ',' = "%2C"
    encodeChar '&' = "%26"
    encodeChar c   = [c]
