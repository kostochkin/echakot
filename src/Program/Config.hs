module Program.Config where

import Config.Messenger.StdIO
import Config.App
import Log.Logger
import Log.Message
import qualified System.IO as SIO
import Text.Read
import Language.Config
import Prelude hiding (lookup, init)


initLoggingConfig :: (Show a, Show c, Read c) => ConfigLang String c (IO (Logger IO a ()))
initLoggingConfig = do
    c <- init "Logging"
    ls <- lookup c "loggers"
    ls' <- mapM readLogger ls
    return $ fmap joinLoggers $ sequence ls'

readLogger :: (Show a) => c -> ConfigLang String c (IO (Logger IO a ()))
readLogger c = do
    t <- lookupString c "type"
    ll <- lookupString c "logLevel"
    let lggr = flip loggerFromString ll
    case t of
        "stdio" -> returnConfig $ return $ lggr print
        "file" -> do
            fn <- lookupString c "filename"
            returnConfig $ fmap lggr $ initFileLogger fn
        _ -> do
            returnConfig $ return $ loggerFromString (return . const ()) "None"

readAppConfig :: (Show c, Read c) => ConfigLang String c (IO (App Int StdioMessenger))
readAppConfig = do
    c <- init "Application"
    bg <- lookup c "botGeneral"
    i <- lookupDefault bg "repeats" 1
    rm <- lookupString bg "repeatsMessage" 
    hm <- lookupString bg "helpMessage" 
    mc <- lookup c "messenger"
    let kbd = [1..5]
    appC <- getAppConstructor mc
    returnConfig $ appC hm rm kbd i

type AppConstructor = String -> String -> [Int] -> Int -> IO (App Int StdioMessenger)

getAppConstructor :: c -> ConfigLang String c AppConstructor
getAppConstructor c = do
    t <- lookupString c "type"
    m <- lookupString c "showingKeyboardMessage"
    case t of
        "stdio" -> returnConfig $ newMessenger m
        s       -> failConfig $ "Unknown messenger: " ++ s

initFileLogger :: (Show a) => String -> IO (LogMessage a -> IO ())
initFileLogger fn = do
    h <- SIO.openFile fn SIO.AppendMode
    return $ fileLogger h

fileLogger :: Show a => SIO.Handle -> LogMessage a -> IO ()
fileLogger h m = SIO.hPrint h m >> SIO.hFlush h
    
veryFirstLogger :: (Show a) => Logger IO a ()
veryFirstLogger = loggerFromString print "Error"

