module Lib
  ( someFunc
  ) where

import Config.App
import Control.Monad (forever)
import qualified Interpreter.IO as IIO (interpret)
import Log.Logger
--import Log.Message
import Program.Config
import qualified Program.FsdEchoBot as B (botStep)
import qualified Translator.Logger.Bot as BL
import qualified Translator.Logger.IO as IOL
import qualified Translator.Messenger.StdIO as TIO (translate)
import qualified Interpreter.ConfigJson as ICJ
import Data.Aeson (decode)
import Data.String (fromString)


import Interpreter.Actions
--import qualified Interpreter.Test.Config as ITC
--import qualified Interpreter.ConfigJson as CJ
import Prelude hiding (init, lookup)
import qualified Translator.Logger.Config as TLC


someFunc :: IO ()
someFunc = do
  contents <- readFile "botconfig.json"
  let jsconf = maybe (error "AAA") id $ decode $ fromString contents
  let llLevel = "Error"
  Right loggrIO <- ICJ.interpret jsconf (timeTaggedLogger llLevel) (TLC.addDefaultLogging llLevel initLoggingConfig)
  loggr <- loggrIO
  let l = currentLogLevel loggr
  Right appIO <- ICJ.interpret jsconf (tagLogger loggr) (TLC.addDefaultLogging l readAppConfig)
  app <- appIO
  IIO.interpret (messenger app) (tagLogger loggr) $
    forever $
    IOL.addDefaultLogging l $
    TIO.translate app $ BL.addDefaultLogging l B.botStep
  return ()

--logtt :: Logger IO TimeTagged () -> LogMessage String -> IO ()
--logtt l m = do
--  tt <- toTimeTagged
--  runLogger l $ fmap tt $ m
