module Messenger.IO (
      IOMessenger
    , StdioMessenger
    , newMessenger
    , messengerRepeats
    , modifyRepeats
    , helpString
    , keyboardLabels
    , keyboardValues
    ) where

import Control.Monad.IO.Class (
      MonadIO
    , liftIO
    )
import Control.Concurrent.MVar (
      MVar
    , newMVar
    , readMVar
    , takeMVar
    , putMVar
    )

class IOMessenger a where
    newMessenger     :: MonadIO m => Int -> [Int] -> String -> m a
    messengerRepeats :: MonadIO m => a -> id -> m Int
    modifyRepeats    :: MonadIO m => a -> id -> Int -> m ()
    helpString       :: a -> String
    keyboardLabels   :: a -> [String]
    keyboardValues   :: a -> [Int]

    
data StdioMessenger = StdioMessenger {
        stdioRepeats :: MVar Int,
        stdioHelpString :: String,
        stdioKeyboard :: [(Int, String)]
    }

newStdioMessenger :: Int -> [Int] -> String -> IO StdioMessenger
newStdioMessenger i k h = do
        r <- newMVar i
        let kbd = [(x, show x) | x <- k]
        return $ StdioMessenger { stdioRepeats = r, stdioHelpString = h, stdioKeyboard = kbd }
   
instance IOMessenger StdioMessenger where 
    newMessenger i k h   = liftIO $ newStdioMessenger i k h
    messengerRepeats s _ = liftIO $ readMVar $ stdioRepeats s
    modifyRepeats s _ i  = liftIO $ takeMVar (stdioRepeats s) >> putMVar (stdioRepeats s) i
    helpString           = stdioHelpString
    keyboardLabels       = map snd . stdioKeyboard
    keyboardValues       = map fst . stdioKeyboard

