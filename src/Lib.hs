module Lib
  ( someFunc
  ) where

import Config.App
import Control.Monad (forever)
import qualified Interpreter.IO as IIO (interpret)
import Log.Logger
import Log.Message
import Program.Config
import qualified Program.FsdEchoBot as B (botStep)
import qualified Translator.Logger.Bot as BL (addDefaultLogging)
import qualified Translator.Logger.IO as IOL (addDefaultLogging)
import qualified Translator.Messenger.StdIO as TIO (translate)

import Interpreter.Actions
import qualified Interpreter.Test.Config as ITC
--import qualified Interpreter.ConfigJson as CJ
import Prelude hiding (init, lookup)
import qualified Translator.Logger.Config as TLC

loggers :: [[(String, String)]]
loggers =
  [ [("type", "stdio"), ("logLevel", "None")]
  , [("type", "file"), ("logLevel", "Debug"), ("filename", "echobot.log")]
  ]

messengerC :: [(String, String)]
messengerC =
  [ ("type", "stdio")
  , ( "showingKeyboardMessage"
    , "Please, enter one of the following commands to modify repeats")
  ]

configApp :: [(String, String)]
configApp =
  [ ("loggers", show loggers)
  , ("messenger", show messengerC)
  , ("botGeneral", show botGeneral)
  ]

botGeneral :: [(String, String)]
botGeneral =
  [ ("repeats", "1")
  , ("repeatsMessage", "Current repeats")
  , ( "helpMessage"
    , "Hi, I'm an echo bot. My name is Echakot. I know these commands: /repeat, /help. Do you know that?")
  ]

someFunc :: IO ()
someFunc = do
  let (ioLog, Right loggrIO) = ITC.interpret configApp initLoggingConfig
  loggr <- loggrIO
  mapM_ (logtt loggr) ioLog
  let l = currentLogLevel loggr
  case ITC.interpret configApp (TLC.addDefaultLogging l readAppConfig) of
    (ioLog', Right appIO) -> do
      mapM_ (logtt loggr) ioLog'
      app <- appIO
      IIO.interpret (messenger app) loggr $
        forever $
        IOL.addDefaultLogging l $
        TIO.translate app $ BL.addDefaultLogging l B.botStep
      return ()
    (ioLog'', Left e) -> do
      mapM_ (logtt loggr) ioLog''
      error e

logtt :: Logger IO TimeTagged () -> LogMessage String -> IO ()
logtt l m = do
  tt <- toTimeTagged
  runLogger l $ fmap tt $ m
