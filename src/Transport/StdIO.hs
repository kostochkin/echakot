{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transport.StdIO
    ( StdIO
    , runStdIO
    , waitMessage
    , sendMessage
    ) where

import Transport.Api
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

instance Api StdIO where
    waitMessage = StdIO $ putStr "> " >> hFlush stdout >> getLine
    sendMessage = StdIO . putStrLn



