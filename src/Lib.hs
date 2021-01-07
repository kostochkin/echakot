module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import Bot.FsdEcho
import qualified Interpreter.IO as IIO
import qualified Translator.BotLogger as BL (addLogging)

someFunc :: IO ()
someFunc = do
    s <- IIO.newMessenger 1 [1..5] "There is a useless help message" :: IO (IIO.StdioMessenger)
    IIO.interpret s (forever (BL.addLogging botStep))
    return ()

