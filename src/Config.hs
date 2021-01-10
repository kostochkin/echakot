module Config (
      logConfig
    , appConfig
    , appState
    , Config.Logging.logLevelString
    , logger
    ) where

import Config.App
import Config.Logging
import Config.Messenger.StdIO
import qualified Interpreter.IO as IIO
import Control.Concurrent.MVar


logConfig :: (Show a) => IO (LoggingConfig IO a ())
logConfig = do
    putStrLn "Please, enter logLevel [None|Error|Info|Debug] (default Info): "
    l <- getLine
    let ioLogger = IIO.defaultLogger Nothing l
    return $ newLoggingConfig ioLogger l


appConfig :: IO (App Int ())
appConfig = do
    let help = "There is a useless help message"
    let repeats = "Current repeats: "
    let kbd = [1..5]
    return $ newMessenger help repeats kbd

appState :: IO (MVar Int)
appState = do
    putStrLn "Please, enter number of repeats (default 1): "
    s <- getLine
    newMessengerState s

