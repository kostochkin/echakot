module Messenger.Telegram.Config
    ( defaultTelegramConfig, botToken, pollingTimeout, timeout, messagesLimit, limit
    ) where


data PollingTimeout = LongPolling Integer | ShortPolling deriving Show

newtype Limit = Limit Integer deriving Show

timeout x
   | x > 0 = LongPolling x
   | otherwise = ShortPolling

limit = Limit . max 1 . min 100

data TelegramConfig = TelegramConfig
    { url :: String,
      botToken :: String,
      pollingTimeout :: PollingTimeout,
      messagesLimit :: Limit
      
    } deriving Show

defaultTelegramConfig = TelegramConfig
    { url = "https://api.telegram.org",
      botToken = "",
      pollingTimeout = ShortPolling,
      messagesLimit = limit 100
    }

