module Translator.IO ( translate, newMessenger, StdioMessenger ) where

import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Language.Bot
import Language.IO
import Control.Monad.Free
import Interpreter.Actions


translate :: IOMessenger a => a -> BotApi String Int () -> IOApi String Int ()
translate _ (Pure x) = Pure x
translate a (Free bf) = free bf where
    free (EchoMessage str f)    = putIOLine str >> translate a f
    free (GetMessages f)        = getIOLine >>= translate a . f . pure
    free (SelectAction str f)   = return (strToAction str (keyboardValues a)) >>= translate a . f 
    free (ShowKeyboard f)       = do
        putIOLine "Available commands:"
        mapM_ (putIOLine . ("/repeat " ++)) $ keyboardLabels a 
        translate a f
    free (SetRepeats i f)       = runIO (modifyRepeats a () i) >> translate a f
    free (GetCurrentRepeats f)  = returnIO (messengerRepeats a ()) >>= translate a . f
    free (TellCurrentRepeats f) = do
        i <- returnIO (messengerRepeats a ())
        putIOLine $ "Current: " ++ show i
        translate a f
    free (ShowHelp f)           = putIOLine (show (helpString a)) >> translate a f
    free (ApiLog m f)           = rawLog m >> translate a f


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

