module Lib
    ( someFunc
    ) where

import Transport.StdIO
    ( runStdIO
    , StdIO
    , waitMessage
    , sendMessage
    )
import Control.Monad
    ( forever
    )

simpleEchoBot :: StdIO ()
simpleEchoBot = do
    x <- waitMessage
    sendMessage x

someFunc = forever $ runStdIO bot

