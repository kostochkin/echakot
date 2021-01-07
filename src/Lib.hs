module Lib
    ( someFunc
    ) where

import Control.Monad (forever)
import Control.Monad.Free
import Log
import Language.Bot
import Bot.FsdEcho
import qualified Interpreter.IO as IIO



debugBotApi :: BotApi String Int () -> BotApi String Int ()
debugBotApi f@(Pure x)   = apiLog Debug ("Step finished: " ++ show x) >> f
debugBotApi f@(Free b) = maybe f debug (tryLog b) where
    debug x = apiLog Debug x >> free b
    free (EchoMessage str g)    = echoMessage str >> debugBotApi g
    free (GetMessages g)        = getMessages >>= gotValue "messages" >>= debugBotApi . g
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
    s <- IIO.newMessenger 1 [1..5] "There is a useless help message" :: IO (IIO.StdioMessenger)
    IIO.interpret s (forever (debugBotApi botStep))
    return ()

