module Messenger.Telegram.Config
    ( defaultTelegramConfig
    , botToken
    , pollingTimeout
    , timeout
    , updatesLimit
    , limit
    , Query(..)
    , configToQuery
    , TelegramConfig
    ) where


type QueryParam = (String, String)

data Query = Query
    { url :: String
    , query :: [QueryParam]
    } deriving Show

data PollingTimeout = LongPolling Integer | ShortPolling deriving Show

newtype Limit = Limit {getLimit :: Integer} deriving Show

data TelegramConfig = TelegramConfig
    { host :: String
    , botToken :: String
    , pollingTimeout :: PollingTimeout
    , updatesLimit :: Limit
    } deriving Show

defaultTelegramConfig = TelegramConfig
    { host = "api.telegram.org"
    , botToken = ""
    , pollingTimeout = ShortPolling
    , updatesLimit = limit 100
    }

timeout x
   | x > 0 = LongPolling x
   | otherwise = ShortPolling

limit = Limit . max 1 . min 100

configToQuery :: TelegramConfig -> Query
configToQuery c = Query
    { url = "https://" ++ host c ++ "/bot" ++ botToken c ++ "/"
    , query = timeout (pollingTimeout c) ++ updates (getLimit $ updatesLimit c)
    } where
        timeout ShortPolling = []
        timeout (LongPolling t) = [("timeout", show t)]
        updates n = [("updates", show n)]

