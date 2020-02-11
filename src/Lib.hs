module Lib
    ( someFunc
    ) where

import  Messenger.Api as M
    (Api (..)
    )

import Messenger.StdIO
    ( runStdIOMessenger
    )
import Control.Monad
    ( forever
    )

simpleEchoBot :: (M.Api m) => m ()
simpleEchoBot = do
    x <- receiveMessage
    sendMessage x

someFunc = forever $ runStdIOMessenger simpleEchoBot

