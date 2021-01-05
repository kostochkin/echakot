{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import qualified System.IO as SIO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Free

data Action i = Echo | Help | TellRepeat | ModifyRepeat i deriving Show

data BotApiF b i a = GetMessage (b -> a)
                   | GetCurrentRepeats (i -> a)
                   | ShowHelp a
                   | SendMessage b a 
                   | SelectAction b (Action i -> a)
                   | TellCurrentRepeats a
                   | ShowKeyboard a
                   | SetRepeats i a deriving (Functor)

type BotApi b i = Free (BotApiF b i)

sendMessage :: b -> BotApi b i ()
sendMessage b = liftF $ SendMessage b ()

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


showHelp :: BotApi b i ()
showHelp = liftF $ ShowHelp ()


strToAction :: (Read i) => String -> Action i
strToAction s = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ try (reads si)
                    _               -> Echo
    where
        try [(x, "")] = Just x
        try _ = Nothing



listBotApi :: (Read i, Show i) => BotApi String i () -> (i, [String]) -> [String]
listBotApi (Pure x) xs = []
listBotApi (Free f) xs = free f xs where
    free a@(SendMessage str f) s           = str : listBotApi f s
    free (GetMessage _) (_, [])            = []
    free a@(GetMessage f) (i,(x:xs))       = listBotApi (f x) (i, xs)
    free a@(SelectAction b f) s            = listBotApi (f (strToAction b)) s
    free a@(ShowKeyboard f) s              = "keyboard" : listBotApi f s
    free a@(SetRepeats i f) (_, xs)        = listBotApi f (i, xs)
    free a@(GetCurrentRepeats f) s@(i, xs) = listBotApi (f i) s
    free a@(TellCurrentRepeats f) s@(i,_)  = show i : listBotApi f s
    free a@(ShowHelp f) s                  = "help" : listBotApi f s



stdioBotApi :: BotApi String Int () -> IO ()
stdioBotApi (Pure x) = return ()
stdioBotApi (Free f) = free f where
    free a@(SendMessage str t)    = putStrLn str >> stdioBotApi t
    free a@(GetMessage f)         = getLine >>= stdioBotApi . f
    free a@(SelectAction str f)   = return (strToAction str) >>= stdioBotApi . f 
    free a@(ShowKeyboard f)       = putStrLn "Reserverd for keyboard" >> stdioBotApi f
    free a@(SetRepeats i f)       = putStrLn "Just imitation!" >> stdioBotApi f
    free a@(GetCurrentRepeats f)  = stdioBotApi (f 1)
    free a@(TellCurrentRepeats f) = print 1 >> stdioBotApi f
    free a@(ShowHelp f)           = print "Reserved for help" >> stdioBotApi f


botStep :: BotApi String Int ()
botStep = do
    m <- getMessage
    a <- selectAction m
    botAction a m


botAction :: Action Int -> String -> BotApi String Int ()
botAction Help             = const $ showHelp
botAction TellRepeat       = const $ tellCurrentRepeats >> showKeyboard
botAction (ModifyRepeat i) = const $ setRepeats i
botAction Echo             = \m -> do
    i <- getCurrentRepeats
    replicateM i (sendMessage m)
    return ()


botForever :: BotApi String Int ()
botForever = forever botStep

someFunc :: IO ()
someFunc = do
    print $ listBotApi botForever (1, ["/help", "111", "/repeat", "222", "/repeat 2", "/repeat", "333", "/repeat 5", "444", "/repeat -1", "4321"])
    stdioBotApi botForever
    return ()

