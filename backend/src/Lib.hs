{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

type MemoryDiaryAPI = "health" :> Get '[JSON] String

server :: Server MemoryDiaryAPI
server = return "ok"

app :: Application
app = serve (Proxy :: Proxy MemoryDiaryAPI) server

startApp :: IO ()
startApp = do
  putStrLn "Memory Diary starting on port 3000..."
  run 3000 app
