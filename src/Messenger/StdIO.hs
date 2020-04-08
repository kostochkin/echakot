{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Messenger.StdIO
    ( StdIOMessenger
    , runStdIOMessenger
    , M.sendMessage
    , M.receiveMessage
    , M.showKeyboard
    , M.Keyboard
    , M.tryReadKey
    ) where

import Messenger.Api as M
import Transport.StdIO as T

newtype StdIOMessenger a = StdIOMessenger (StdIO a) deriving (Functor, Applicative, Monad)

runStdIOMessenger (StdIOMessenger x) = T.runStdIO x

instance M.Api StdIOMessenger where
    sendMessage x = StdIOMessenger $ T.sendMessage $ M.getMessage x
    receiveMessage kbd = StdIOMessenger $ fmap (parseMsg kbd) $ T.waitMessage
    showKeyboard kbd = StdIOMessenger $ showKeyboard' kbd

showKeyboard' ::  M.Keyboard -> StdIO ()
showKeyboard' = mapM_ sendMessage . M.keys where
    sendMessage = T.sendMessage . ("\\repeat " ++) . show

parseMsg :: Keyboard -> String -> Message
parseMsg = M.parseMessage M.Message tryReadCommand

tryReadCommand :: Keyboard -> String -> Maybe Message
tryReadCommand kbd cmd = 
    case break (== ' ') cmd of
        ("\\repeat", [])    -> Just Repeat
        ("\\repeat", xs)    -> M.tryReadKey kbd xs
        ("\\help", [])      -> Just Help
        _                   -> Nothing

