module Messenger.Api 
    ( Api(..)
    , Message(..)
    , message
    , getMessage
    , keyboard
    , Keyboard
    , keys
    , tryReadKey
    , parseMessage
    , (#)
    ) where

import Data.Maybe (listToMaybe)

class (Monad m) => Api m where
    sendMessage :: Message -> m ()
    receiveMessage :: Keyboard -> m Message
    showKeyboard :: Keyboard -> m ()

newtype Keyboard = Keyboard {keys :: [Int]} deriving (Show)

data Message = Message String | Key Int | Help | Repeat deriving (Show)

message :: String -> Message
message x = Message x

getMessage (Message x) = x

parseMessage :: (a -> Message) -> (Keyboard -> a -> Maybe Message) -> Keyboard -> a -> Message
parseMessage def rd kbd msg = maybe (def msg) id (rd kbd msg)

keyboard :: Keyboard
keyboard = Keyboard []

addKey :: Keyboard -> Int -> Keyboard
addKey (Keyboard ks) k = Keyboard $ ks ++ [k]

infixl 1 #
(#) = addKey

tryReadKey :: Keyboard -> String -> Maybe Message
tryReadKey kbd = fmap (Key . fst) . listToMaybe . filter isKey . reads where
    isKey = (&&) <$> isPure <*> inKeyboard kbd
    isPure = (== []) . snd
    inKeyboard kbd = (`elem` keys kbd) . fst

