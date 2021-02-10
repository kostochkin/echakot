{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Program.Config where

import Config.App
import Config.Messenger.StdIO
import Language.Config
import Log.Logger
import Log.Message
import Prelude hiding (init, lookup)
import qualified System.IO as SIO

initLoggingConfig :: (Show a, Parsable (Val c) LoggerType, Parsable (Val c) String, Parsable (Val c) [c], Semiredis c, Show (Val c), Show c) => ConfigLang c (IO (Logger IO a ()))
initLoggingConfig = do
  c <- init "Logging"
  ls <- withDefault [] $ lookup c "loggers"
  ls' <- mapM readLogger ls
  return $ fmap joinLoggers $ sequence ls'

readLogger :: (Show a, Parsable (Val c) LoggerType, Parsable (Val c) String, Semiredis c, Show (Val c), Show c) => c -> ConfigLang c (IO (Logger IO a ()))
readLogger c = do
  t <- lookup c "type"
  ll <- lookup c "logLevel"
  let lggr = flip loggerFromString ll
  case t of
    Stdio -> returnConfig $ return $ lggr print
    File -> do
      fn <- lookup c "filename"
      returnConfig $ fmap lggr $ initFileLogger fn

readAppConfig :: (Parsable (Val c) Int, Parsable (Val c) c, Parsable (Val c) String, Semiredis c, Show (Val c), Show c) => ConfigLang c (IO (App Int StdioMessenger))
readAppConfig = do
  c <- init "Application"
  bg <- lookup c "botGeneral"
  let kbd = [1 .. 5]
  i <- withDefault 1 (lookup bg "repeats" >>= validate (`elem` kbd))
  rm <- lookup bg "repeatsMessage"
  hm <- lookup bg "helpMessage"
  mc <- lookup c "messenger"
  appC <- getAppConstructor mc
  returnConfig $ appC hm rm kbd i

type AppConstructor
   = String -> String -> [Int] -> Int -> IO (App Int StdioMessenger)

getAppConstructor :: (Semiredis c, Parsable (Val c) String, Parsable (Val c) c, Show (Val c)) => c -> ConfigLang c AppConstructor
getAppConstructor c = do
  t <- lookup c "type"
  m <- lookup c "showingKeyboardMessage"
  case t of
    "stdio" -> returnConfig $ newMessenger m
    s -> failConfig $ "Unknown messenger: " ++ s

initFileLogger :: (Show a) => String -> IO (LogMessage a -> IO ())
initFileLogger fn = do
  h <- SIO.openFile fn SIO.AppendMode
  return $ fileLogger h

fileLogger :: Show a => SIO.Handle -> LogMessage a -> IO ()
fileLogger h m = SIO.hPrint h m >> SIO.hFlush h

