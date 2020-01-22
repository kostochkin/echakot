module Messenger.Telegram.Config
    ( defaultTelegramConfig
    , botToken
    , pollingTimeout
    , timeout
    , updatesLimit
    , limit
    ) where


data PollingTimeout = LongPolling Integer | ShortPolling deriving Show

newtype Limit = Limit {getLimit :: Integer} deriving Show

data TelegramConfig = TelegramConfig
    { url :: String
    , botToken :: String
    , pollingTimeout :: PollingTimeout
    , updatesLimit :: Limit
    } deriving Show

defaultTelegramConfig = TelegramConfig
    { url = "https://api.telegram.org"
    , botToken = ""
    , pollingTimeout = ShortPolling
    , updatesLimit = limit 100
    }

timeout x
   | x > 0 = LongPolling x
   | otherwise = ShortPolling

limit = Limit . max 1 . min 100

