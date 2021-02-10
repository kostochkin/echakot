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
import Data.Aeson (decode, Value)
import Data.String (fromString)


import Interpreter.Actions
--import qualified Interpreter.Test.Config as ITC
import qualified Interpreter.ConfigJson as CJ
import Prelude hiding (init, lookup)
import qualified Translator.Logger.Config as TLC
import qualified Control.Exception as CE
import System.Exit (exitWith, ExitCode (ExitFailure))
import qualified Interpreter.Config as SIC


veryFirstLogger :: Logger IO String ()
veryFirstLogger = timeTaggedLogger "Warn"

veryFirstLogger' :: Logger IO String ()
veryFirstLogger' = timeTaggedLogger "Debug"


readJSONLoggingConfig :: Value -> Logger IO String () -> IO (Either String (Logger IO String ()))
readJSONLoggingConfig v l = do
    let lLevel = currentLogLevel l
    el <- ICJ.interpret v l $ TLC.addDefaultLogging lLevel initLoggingConfig
    traverse (fmap tagLogger) el
            

readJSONFile :: String -> IO (Either String Value)
readJSONFile f = do
    contents <- readFile f
    let d = decode $ fromString contents
    return $ maybe (Left "Wrong syntax") Right d
    

catchIOEither :: IO (Either String a) -> IO (Either String a)
catchIOEither c = do
    c `CE.catch` (\e -> return $ Left $ "IO error: " ++ show (e :: CE.IOException))


loggedIO :: Logger IO String () -> String -> IO (Either String a) -> IO (Either String a)
loggedIO l s io = do
    a <- inlineLogDebug ("RunIO: " ++ s) l *>> catchIOEither io
    case a of
        Right x -> inlineLogDebug ("OkIO: " ++ s) l *>> return (Right x)
        Left x -> inlineLogError (s ++ ": " ++ x) l *>> return (Left x)

safeExit :: IO (Either String a) -> IO (Either String a)
safeExit x = x >>= \e -> case e of
    Left _  -> exitWith $ ExitFailure 1
    Right a -> return $ Right a
    

someFunc :: IO ()
someFunc = do
  let jsfile = readJSONFile "botconfig.json"
  Right vvv <- jsfile
  _ <- SIC.interpret (CJ.JsonVal vvv) veryFirstLogger' initLoggingConfig :: IO (Maybe (IO (Logger IO String ())))
  Right jsconf <- safeExit $ loggedIO veryFirstLogger "Read config file" $ jsfile
  Right loggr <- safeExit $ loggedIO veryFirstLogger "Read logging config" $  readJSONLoggingConfig jsconf veryFirstLogger
  let l = currentLogLevel loggr
  Right appIO <- safeExit $ loggedIO loggr "Read app config" $ ICJ.interpret jsconf loggr (TLC.addDefaultLogging l readAppConfig)
  app <- appIO
  IIO.interpret (messenger app) loggr $
    forever $
    IOL.addDefaultLogging l $
    TIO.translate app $ BL.addDefaultLogging l B.botStep
  return ()

--logtt :: Logger IO TimeTagged () -> LogMessage String -> IO ()
--logtt l m = do
--  tt <- toTimeTagged
--  runLogger l $ fmap tt $ m
