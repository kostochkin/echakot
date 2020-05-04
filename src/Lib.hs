module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import Bot.FsdEcho (fsdEchoBotStep)
import Messenger.StdIO (runStdIOMessengerLogged)
import qualified Bot.StateClass as B
import qualified Logger as L


someFunc = do
    let init_state = B.BotSt {B.repeats = 1}
    let bot = forever $ fsdEchoBotStep
    let final_state = L.runLogger $ B.runBotStateT bot init_state
    runStdIOMessengerLogged L.runLogger final_state
    return ()

