module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import qualified Bot.FsdEcho as B (botStep)
import qualified Interpreter.IO as IIO (interpret)
import qualified Translator.BotLogger as BL (addDefaultLogging)
import qualified Translator.IOLogger as IOL (addDefaultLogging)
import qualified Translator.IO as TIO (translate)
import qualified Messenger.IO as MIO (newMessenger, StdioMessenger)

someFunc :: IO ()
someFunc = do
    let l = "Debug"
    m <- MIO.newMessenger 1 [1..5] "There is a useless help message" :: IO (MIO.StdioMessenger)
    IIO.interpret $ forever $ IOL.addDefaultLogging l $ TIO.translate m $ BL.addDefaultLogging l B.botStep
    return ()


