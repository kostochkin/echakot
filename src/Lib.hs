module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import qualified Program.FsdEchoBot as B (botStep)
import qualified Interpreter.IO as IIO (interpret)
import qualified Translator.Logger.Bot as BL (addDefaultLogging)
import qualified Translator.Logger.IO as IOL (addDefaultLogging)
import qualified Translator.Messenger.StdIO as TIO (translate)
import Log.Logger
import Program.Config
import Config.App
import Log.Message

import Language.Config
--import qualified Interpreter.ConfigJson as CJ
import Prelude hiding (lookup, init)
import Config
import Interpreter.Actions


someFunc :: IO ()
someFunc = do
    let Right loggrIO = dummyInterpret configApp initLoggingConfig
    loggr <- loggrIO
    let l = currentLogLevel loggr
    case dummyInterpret configApp readAppConfig of
        Right appIO -> do
            app <- appIO
            IIO.interpret (messenger app) loggr $ forever $ IOL.addDefaultLogging l $ TIO.translate app $ BL.addDefaultLogging l B.botStep
            return ()
        Left e -> do
            tt <- toTimeTagged
            runLogger loggr $ fmap tt $ messageError e


