{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import qualified System.IO as SIO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Free
import Control.Concurrent.MVar

data LogLevel = None | Debug | Error deriving (Eq, Ord, Show)

class Loggable a where
    tryLog   :: a -> Maybe String
    logLevel :: LogLevel -> a -> Maybe String
    logDebug :: a -> Maybe String
    logError :: a -> Maybe String
    logLevel l a = fmap (("[" ++ show l ++ "] ") ++) $ tryLog a
    logDebug = logLevel Debug
    logError = logLevel Error

data Action i = Echo | Help | TellRepeat | ModifyRepeat i deriving Show


data BotApiF b i a = GetMessage (b -> a)
                   | GetCurrentRepeats (i -> a)
                   | ShowHelp a
                   | EchoMessage b a 
                   | SelectAction b (Action i -> a)
                   | TellCurrentRepeats a
                   | ShowKeyboard a
                   | ApiLog LogLevel String a
                   | SetRepeats i a deriving (Functor)

instance (Show b, Show i) => Loggable (BotApiF b i a) where
    tryLog (ApiLog _ _ _) = Nothing
    tryLog f              = Just $ fmtApi f


fmtApi :: (Show b, Show i) => BotApiF b i a -> String
fmtApi = log where
    log (GetMessage _)         = "Wait for a message ... "
    log (GetCurrentRepeats _ ) = "Getting current repeats"
    log (ShowHelp _)           = "Showing help"
    log (EchoMessage b _)      = "Echoing the message " ++ show b
    log (SelectAction b _)     = "Selecting action for message" ++ show b
    log (TellCurrentRepeats _) = "Telling current repeats"
    log (ShowKeyboard _)       = "Showing keyboard"
    log (SetRepeats i _)       = "Setting repeats to " ++ show i


type BotApi b i = Free (BotApiF b i)

echoMessage :: b -> BotApi b i ()
echoMessage b = liftF $ EchoMessage b ()

getMessage :: BotApi b i b
getMessage = liftF $ GetMessage id 

selectAction :: b -> BotApi b i (Action i)
selectAction b = liftF $ SelectAction b id

showKeyboard :: BotApi b i ()
showKeyboard = liftF $ ShowKeyboard ()

setRepeats :: i -> BotApi b i ()
setRepeats i = liftF $ SetRepeats i ()

tellCurrentRepeats :: BotApi b i ()
tellCurrentRepeats = liftF $ TellCurrentRepeats ()

getCurrentRepeats :: BotApi b i i
getCurrentRepeats = liftF $ GetCurrentRepeats id

apiLog :: LogLevel -> String -> BotApi b i ()
apiLog l m = liftF $ ApiLog l ("[Bot] " ++ m) ()


showHelp :: BotApi b i ()
showHelp = liftF $ ShowHelp ()


strToAction :: (Read i, Eq i) => String -> [i] -> Action i
strToAction s k = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ try (reads si)
                    _               -> Echo
    where
        try [(x, "")] | x `elem` k = Just x
        try _                      = Nothing



listBotApi :: (Read i, Show i, Eq i) => BotApi String i () -> (i, [String]) -> [i] -> [String]
listBotApi (Pure x) xs k = []
listBotApi (Free f) xs k = free f xs where
    free a@(EchoMessage str f) s           = str : listBotApi f s k
    free (GetMessage _) (_, [])            = []
    free a@(GetMessage f) (i,(x:xs))       = listBotApi (f x) (i, xs) k
    free a@(SelectAction b f) s            = listBotApi (f (strToAction b k)) s k
    free a@(ShowKeyboard f) s              = "keyboard" : listBotApi f s k
    free a@(SetRepeats i f) (_, xs)        = listBotApi f (i, xs) k
    free a@(GetCurrentRepeats f) s@(i, xs) = listBotApi (f i) s k
    free a@(TellCurrentRepeats f) s@(i,_)  = show i : listBotApi f s k
    free a@(ShowHelp f) s                  = "help" : listBotApi f s k
    free a@(ApiLog _ _ f) s              = listBotApi f s k



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


botStep :: BotApi String Int ()
botStep = do
    m <- getMessage
    a <- selectAction m
    botAction a m


botAction :: Action Int -> String -> BotApi String Int ()
botAction Help             = const $ showHelp
botAction TellRepeat       = const $ tellCurrentRepeats >> showKeyboard
botAction (ModifyRepeat i) = const $ setRepeats i
botAction Echo             = \m -> getCurrentRepeats >>= flip replicateM (echoMessage m) >> return ()


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
    print $ listBotApi (forever botStep) (1, ["/help", "111", "/repeat", "222", "/repeat 2", "/repeat", "333", "/repeat 3", "444", "/repeat -1", "555", "/repeat 10", "666"]) [1 .. 5] == ["help", "111", "1", "keyboard", "222", "2", "keyboard", "333", "333", "444", "444", "444", "/repeat -1", "/repeat -1", "/repeat -1", "555", "555", "555", "/repeat 10", "/repeat 10", "/repeat 10", "666", "666", "666"]
    s <- newStdioMessenger 1 [1..5] "There is a useless help message" 
    ioBotApi s (forever (debugBotApi botStep))
    return ()

