{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Geocoding proxy handler for api geocode
--
-- Forwards search queries to the Nominatim API and returns the raw JSON result
module Handler.Geocode
  ( geocodeServer,
  )
where

import qualified Api
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL -- for decoding the JSON response from Nominatim
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE -- converts Text to ByteString (required by urlEncode) and back
import Handler.Helpers (AppM)
import Network.HTTP.Simple (Response, getResponseBody, httpLBS, parseRequest, setRequestHeader) -- for making the HTTP request to Nominatim
import Network.HTTP.Types.URI (urlEncode) -- for encoding the search query in the URL
import Servant (ServerT)

-- | Servant server for the geocode proxy endpoint
geocodeServer :: ServerT Api.GeocodeApi AppM
geocodeServer = geocodeProxy

-- | Forward a search query to Nominatim and return the raw JSON result
geocodeProxy :: Maybe Text -> AppM A.Value
geocodeProxy Nothing = pure (A.toJSON ([] :: [A.Value]))
geocodeProxy (Just query) = liftIO $ do
  let encoded = TE.decodeUtf8 (urlEncode False (TE.encodeUtf8 query)) -- text to bytestring to url-encoded bytestring and back to text
      url =
        "https://nominatim.openstreetmap.org/search?q="
          <> T.unpack encoded -- text to string
          <> "&format=json&limit=8&addressdetails=1"
  request <- parseRequest url -- HTTP request from the URL string
  res <-
    try (httpLBS (setRequestHeader "User-Agent" ["MemoryDiary/1.0"] request)) ::
      IO (Either SomeException (Response BSL.ByteString))
  case res of
    Left _ -> pure (A.toJSON ([] :: [A.Value])) -- network error, return empty array (server should not crash due to external API issues)
    Right output ->
      pure $ fromMaybe (A.toJSON ([] :: [A.Value])) (A.decode (getResponseBody output)) -- decode the JSON response from Nominatim, return empty array if decoding fails for any reason
