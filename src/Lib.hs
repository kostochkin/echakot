module Lib
    ( someFunc
    ) where

import Control.Monad.State
import Control.Monad.Free
import Log
import Bot.Api
import Bot.FsdEcho
import qualified Interpreters.BotApiTest as BAT
import qualified Interpreters.IO as IIO



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
    print $ BAT.interpret (forever botStep) (1, ["/help", "111", "/repeat", "222", "/repeat 2", "/repeat", "333", "/repeat 3", "444", "/repeat -1", "555", "/repeat 10", "666"]) [1 .. 5] == ["help", "111", "1", "keyboard", "222", "2", "keyboard", "333", "333", "444", "444", "444", "/repeat -1", "/repeat -1", "/repeat -1", "555", "555", "555", "/repeat 10", "/repeat 10", "/repeat 10", "666", "666", "666"]
    s <- IIO.newMessenger 1 [1..5] "There is a useless help message" :: IO (IIO.StdioMessenger)
    IIO.interpret s (forever (debugBotApi botStep))
    return ()

