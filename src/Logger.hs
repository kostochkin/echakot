{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logger where

import qualified Messenger.Api as M
import qualified Bot.StateClass as B
import Control.Monad.Trans
import qualified Transport.Api as T

data LogLevel = Off | Debug | Notice | Warning | Error deriving (Eq, Ord, Show)

data LogItem = LogItem LogLevel String

newtype Logger m a = Logger { runLogger :: m a } deriving (Functor, Applicative, Monad)

logDebug = LogItem Debug
logNotice = LogItem Notice
logWarning = LogItem Warning
logError = LogItem Error

instance (M.Api m) => M.Api (Logger m) where
    sendMessage m = Logger $ do
        M.sendMessage $ M.Message $ "[Api] Sent: " ++ show m
        M.sendMessage m
    receiveMessage kbd = Logger $ do
        m <- M.receiveMessage kbd
        M.sendMessage $ M.Message $ "[Api] Got: " ++ show m
        return m
    showKeyboard kbd = Logger $ do
        M.sendMessage $ M.Message $ "[Api] Show keyboard: " ++ show kbd
        M.showKeyboard kbd

instance (Monad m, T.Api m) => T.Api (Logger m) where
    waitMessage = Logger $ do
        T.sendMessage $ "[Transport] Wait message" 
        T.waitMessage
    sendMessage m = Logger $ do
        T.sendMessage $ "[Transport] Send message: " ++ m 
        T.sendMessage m

