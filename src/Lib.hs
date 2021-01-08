module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import qualified Bot.FsdEcho as B (botStep)
import qualified Interpreter.IO as IIO (interpret)
import qualified Translator.BotLogger as BL (addLogging)
import qualified Translator.IOLogger as IOL (addLogging)
import qualified Translator.IO as TIO (translate, newMessenger, StdioMessenger)
import Log.Logger (loggerFromString)

someFunc :: IO ()
someFunc = do
    let logger = loggerFromString "Debug"
    m <- TIO.newMessenger 1 [1..5] "There is a useless help message" :: IO (TIO.StdioMessenger)
    IIO.interpret $ forever $ IOL.addLogging logger $ TIO.translate m $ BL.addLogging logger B.botStep
    return ()

