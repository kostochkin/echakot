{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transport.StdIO
    ( StdIO
    , runStdIO
    , T.waitMessage
    , T.sendMessage
    ) where

import qualified Messenger.Api as M

import qualified Transport.Api as T
    ( Api
    , waitMessage
    , sendMessage
    ) 

import System.IO
    ( hFlush
    , stdout
    )

newtype StdIO a = StdIO
    { runStdIO :: IO a
    } deriving (Functor, Applicative, Monad)

instance T.Api StdIO where
    waitMessage = StdIO $ putStr "> " >> hFlush stdout >> getLine
    sendMessage = StdIO . putStrLn

instance M.Api StdIO where
    sendMessage = undefined
    receiveMessage kbd = undefined
    showKeyboard kbd = undefined

