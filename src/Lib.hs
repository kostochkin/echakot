module Lib
  ( someFunc
  ) where

import Config.App
import Control.Monad (forever, join)
import qualified Interpreter.IO as IIO (interpret)
import qualified Interpreter.GenIOStdio as IGIO (interpret)
import qualified Translator.Messenger.GenIOStdIO as TMGIO (translate)
import Log.Logger
--import Log.Message
import Program.Config
import qualified Program.FsdEchoBot as B (botStep)
import qualified Translator.Logger.Bot as BL
import qualified Translator.Logger.IO as IOL
import qualified Translator.Messenger.StdIO as TIO (translate)
import qualified Interpreter.ConfigJson as ICJ
import Data.Aeson (decode, Value)
import Data.String (fromString)


import Interpreter.Actions
--import qualified Interpreter.Test.Config as ITC
--import qualified Interpreter.ConfigJson as CJ
import Prelude hiding (init, lookup)
import qualified Translator.Logger.Config as TLC
import System.Exit (exitWith, ExitCode (ExitFailure))
import qualified Interpreter.Config as SIC
import qualified Control.Monad.Catch as CMC


veryFirstLogger :: Logger IO String ()
veryFirstLogger = timeTaggedLogger "Warn"


readJSONLoggingConfig :: (Show a) => Value -> Logger IO String () -> IO (Logger IO a ())
readJSONLoggingConfig v l = join $ ICJ.interpret v l initLoggingConfig
    

readJSONFile :: String -> IO Value
readJSONFile f = do
    contents <- readFile f
    let d = decode $ fromString contents
    maybe (SIC.throwIE "Wrong syntax") return d
    


loggedIO :: Logger IO String () -> String -> IO a -> IO a
loggedIO l s io = inlineLogDebug ("RunIO: " ++ s) l *>> io

maybeFail :: IO a -> IO a
maybeFail x = x `CMC.catch` exitOnException 1

exitOnException :: Int -> CMC.SomeException -> IO a
exitOnException x = const $ exitWith $ ExitFailure x
    

someFunc :: IO ()
someFunc = do
  let jsfile = readJSONFile "botconfig.json"
  jsconf <- maybeFail $ loggedIO veryFirstLogger "Read config file" $ jsfile
  let confReaded = readJSONLoggingConfig jsconf veryFirstLogger
  loggr <- maybeFail $ loggedIO veryFirstLogger "Read logging config" $ confReaded
  let l = currentLogLevel loggr
  let tloggr = tagLogger loggr
  appIO <- maybeFail $ loggedIO tloggr "Read app config" $ ICJ.interpret jsconf tloggr (TLC.addDefaultLogging l readAppConfig)
  app <- appIO
  IGIO.interpret (messenger app) tloggr $
    forever $
    TMGIO.translate app $ BL.addDefaultLogging l B.botStep
  IIO.interpret (messenger app) tloggr $
    forever $
    IOL.addDefaultLogging l $
    TIO.translate app $ BL.addDefaultLogging l B.botStep
  return ()

--logtt :: Logger IO TimeTagged () -> LogMessage String -> IO ()
--logtt l m = do
--  tt <- toTimeTagged
--  runLogger l $ fmap tt $ m
