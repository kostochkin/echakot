module Transport.Api
    ( Api(..)
    ) where

class Api m where
    waitMessage :: m (String)
    sendMessage :: String -> m ()
