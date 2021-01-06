module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import qualified System.IO as SIO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Free
import Control.Concurrent.MVar
import Log
import Bot.Api
import Bot.FsdEcho
import qualified Interpreters.List as IL



strToAction :: (Read i, Eq i) => String -> [i] -> Action i
strToAction s k = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ try (reads si)
                    _               -> Echo
    where
        try [(x, "")] | x `elem` k = Just x
        try _                      = Nothing

ioBotApi :: IOMessenger a => a -> BotApi String Int () -> IO ()
ioBotApi _ (Pure x) = return ()
ioBotApi a (Free f) = free f where
    free (EchoMessage str f)    = putStrLn str >> ioBotApi a f
    free (GetMessage f)         = getLine >>= ioBotApi a . f 
    free (SelectAction str f)   = return (strToAction str (keyboardValues a)) >>= ioBotApi a . f 
    free (ShowKeyboard f)       = do
        putStrLn "Available commands:"
        mapM_ (putStrLn . ("/repeat " ++)) $ keyboardLabels a 
        ioBotApi a f
    free (SetRepeats i f)       = modifyRepeats a () i >> ioBotApi a f
    free (GetCurrentRepeats f)  = messengerRepeats a () >>= ioBotApi a . f
    free (TellCurrentRepeats f) = messengerRepeats a () >>= (putStrLn . ("Current: " ++) . show) >> ioBotApi a f
    free (ShowHelp f)           = print (helpString a) >> ioBotApi a f
    free (ApiLog l m f)         = putStrLn ("[" ++ show l ++ "] " ++ m) >> ioBotApi a f



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




debugBotApi :: BotApi String Int () -> BotApi String Int ()
debugBotApi f@(Pure x)   = apiLog Debug ("Step finished: " ++ show x) >> f
debugBotApi f@(Free b) = maybe f debug (tryLog b) where
    debug x = apiLog Debug x >> free b
    free (EchoMessage str g)    = echoMessage str >> debugBotApi g
    free (GetMessage g)         = getMessage >>= gotValue "message" >>= debugBotApi . g 
    free (SelectAction str g)   = selectAction str >>= gotValue "action" >>= debugBotApi . g
    free (ShowKeyboard g)       = showKeyboard >> debugBotApi g
    free (SetRepeats i g)       = setRepeats i >> debugBotApi g
    free (GetCurrentRepeats g)  = getCurrentRepeats >>= gotValue "repeats" >>= debugBotApi . g
    free (TellCurrentRepeats g) = tellCurrentRepeats >> debugBotApi g
    free (ShowHelp g)           = showHelp >> debugBotApi g
    free (ApiLog _ _ g)         = debugBotApi g
    gotValue s x = do
        apiLog Debug ("Got " ++ s ++ ": " ++ show x)
        return x
    

someFunc :: IO ()
someFunc = do
    print $ IL.interpret (forever botStep) (1, ["/help", "111", "/repeat", "222", "/repeat 2", "/repeat", "333", "/repeat 3", "444", "/repeat -1", "555", "/repeat 10", "666"]) [1 .. 5] == ["help", "111", "1", "keyboard", "222", "2", "keyboard", "333", "333", "444", "444", "444", "/repeat -1", "/repeat -1", "/repeat -1", "555", "555", "555", "/repeat 10", "/repeat 10", "/repeat 10", "666", "666", "666"]
    s <- newStdioMessenger 1 [1..5] "There is a useless help message" 
    ioBotApi s (forever (debugBotApi botStep))
    return ()

