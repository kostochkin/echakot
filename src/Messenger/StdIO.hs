{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Messenger.StdIO
    ( StdIOMessenger
    , runStdIOMessenger
    , runStdIOMessengerLogged
    , M.sendMessage
    , M.receiveMessage
    , M.showKeyboard
    , M.Keyboard
    , M.tryReadKey
    ) where

import qualified Messenger.Api as M
import qualified Transport.StdIO as Tr
import qualified Transport.Api as T

newtype StdIOMessenger m a = StdIOMessenger (m a) deriving (Functor, Applicative, Monad)

runStdIOMessenger (StdIOMessenger x) = Tr.runStdIO x

runStdIOMessengerLogged logger (StdIOMessenger x) = Tr.runStdIO $ logger x

instance (T.Api m, M.Api m) => M.Api (StdIOMessenger m) where
    sendMessage = StdIOMessenger . T.sendMessage . M.getMessage
    receiveMessage kbd = StdIOMessenger $ fmap (parseMsg kbd) $ T.waitMessage
    showKeyboard kbd = StdIOMessenger $ showKeyboard' kbd

showKeyboard' ::  forall m. (T.Api m, Monad m) => M.Keyboard -> m ()
showKeyboard' = mapM_ sendMessage . M.keys where
    sendMessage = T.sendMessage . ("\\repeat " ++) . show

parseMsg :: M.Keyboard -> String -> M.Message
parseMsg = M.parseMessage M.Message tryReadCommand

tryReadCommand :: M.Keyboard -> String -> Maybe M.Message
tryReadCommand kbd cmd = 
    case break (== ' ') cmd of
        ("\\repeat", [])    -> Just M.Repeat
        ("\\repeat", xs)    -> M.tryReadKey kbd xs
        ("\\help", [])      -> Just M.Help
        _                   -> Nothing

