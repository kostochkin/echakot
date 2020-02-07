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

newtype StdIO a = StdIO
    { runStdIO :: IO a
    } deriving (Functor, Applicative, Monad)

instance Api StdIO where
    waitMessage = StdIO getLine
    sendMessage = StdIO . putStrLn


