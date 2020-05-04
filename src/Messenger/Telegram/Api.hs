module Messenger.Telegram.Api
    ( sendMessage
    , readMessage
    ) where

import Messenger.Telegram.Config (TelegramConfig, configToQuery, Query(..))

data User

data Update = Update
    { update_id :: Integer
    , message :: Maybe Message
    }

data Message = Message
    { message_id :: Integer
    , from :: User
    , date :: Integer
    }

sendMessage :: TelegramConfig -> String -> IO ()
sendMessage = undefined


readMessage :: TelegramConfig -> IO (Message)
readMessage = undefined

