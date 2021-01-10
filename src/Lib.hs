module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import qualified Bot.FsdEcho as B (botStep)
import qualified Interpreter.IO as IIO (interpret)
import qualified Translator.Logger.Bot as BL (addDefaultLogging)
import qualified Translator.Logger.IO as IOL (addDefaultLogging)
import qualified Translator.Messenger.StdIO as TIO (translate)
import Config

someFunc :: IO ()
someFunc = do
    lConfig <- logConfig
    let l = logLevelString lConfig
    let loggr = logger lConfig
    s <- appState
    app <- appConfig
    IIO.interpret s loggr $ forever $ IOL.addDefaultLogging l $ TIO.translate app $ BL.addDefaultLogging l B.botStep
    return ()

-- fileLogger :: Show a => SIO.Handle -> LogMessage a -> IO ()
-- fileLogger h m = SIO.hPrint h m >> SIO.hFlush h
    

