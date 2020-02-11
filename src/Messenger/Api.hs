module Messenger.Api 
    ( Api(..)
    , message
    , getMessage
    ) where

class (Monad m) => Api m where
    sendMessage :: Message -> m ()
    receiveMessage :: m (Message)
    createKeyboard :: m ()

data Message = Message String | Keyboard [String]

message :: String -> Message
message x = Message x

getMessage (Message x) = x

keyboard :: Message
keyboard = Keyboard []

addKey :: Message -> String -> Message
addKey (Keyboard ks) k = Keyboard $ k:ks

infixl 0 #
(#) = addKey

