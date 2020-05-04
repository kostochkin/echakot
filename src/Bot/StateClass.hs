{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.StateClass
    ( BotStateClass(..)
    , BotStateT
    , BotSt(..)
    , runBotStateT
    ) where

import Control.Monad.State
import qualified Messenger.Api as M

class (Monad m) => BotStateClass m where
    getRepeats :: m Int
    modifyRepeats :: (Int -> Int) -> m ()
    setRepeats :: Int -> m ()
    setRepeats = modifyRepeats . const

data BotSt = BotSt {repeats :: Int}

newtype BotStateT m a = BotStateT {runBotStateT' :: (StateT BotSt m a)}
    deriving (Functor, Applicative, Monad, MonadTrans)
 
runBotStateT = runStateT . runBotStateT'

instance (Monad m) => BotStateClass (BotStateT m) where
    getRepeats = BotStateT $ get >>= return . repeats
    modifyRepeats f = BotStateT $ do
        s <- get
        put $ s {repeats = f $ repeats s}

instance M.Api m => M.Api (BotStateT m) where
    sendMessage = BotStateT . lift . M.sendMessage
    receiveMessage = BotStateT . lift . M.receiveMessage
    showKeyboard = BotStateT . lift . M.showKeyboard

