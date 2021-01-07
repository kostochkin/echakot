module Interpreter.IO ( interpret, newMessenger, StdioMessenger ) where

import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Language.Bot
import Control.Monad.Free
import Interpreter.Actions

interpret :: IOMessenger a => a -> BotApi String Int () -> IO ()
interpret _ (Pure _) = return ()
interpret a (Free bf) = free bf where
    free (EchoMessage str f)    = putStrLn str >> interpret a f
    free (GetMessages f)        = getLine >>= interpret a . f . pure
    free (SelectAction str f)   = return (strToAction str (keyboardValues a)) >>= interpret a . f 
    free (ShowKeyboard f)       = do
        putStrLn "Available commands:"
        mapM_ (putStrLn . ("/repeat " ++)) $ keyboardLabels a 
        interpret a f
    free (SetRepeats i f)       = modifyRepeats a () i >> interpret a f
    free (GetCurrentRepeats f)  = messengerRepeats a () >>= interpret a . f
    free (TellCurrentRepeats f) = messengerRepeats a () >>= (putStrLn . ("Current: " ++) . show) >> interpret a f
    free (ShowHelp f)           = print (helpString a) >> interpret a f
    free (ApiLog m f)           = putStrLn (show m) >> interpret a f


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
        stdioKeyboard :: M.Map Int String
    }

newStdioMessenger :: Int -> [Int] -> String -> IO StdioMessenger
newStdioMessenger i k h = do
        r <- newMVar i
        let kbd = M.fromList [(x, show x) | x <- k]
        return $ StdioMessenger { stdioRepeats = r, stdioHelpString = h, stdioKeyboard = kbd }
   
instance IOMessenger StdioMessenger where 
    newMessenger i k h   = liftIO $ newStdioMessenger i k h
    messengerRepeats s _ = liftIO $ readMVar $ stdioRepeats s
    modifyRepeats s _ i  = liftIO $ takeMVar (stdioRepeats s) >> putMVar (stdioRepeats s) i
    helpString           = stdioHelpString
    keyboardLabels       = M.elems . stdioKeyboard
    keyboardValues       = M.keys  . stdioKeyboard

