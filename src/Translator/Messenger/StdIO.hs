module Translator.Messenger.StdIO (
      translate
    , IOApiLang
    ) where

import Language.Bot (
      BotApi
    , BotApiF(..)
    )
import Language.IO (
      IOApi
    , getIOLine
    , putIOLine
    , getState
    , putState
    , rawLog
    )    
import Control.Monad.Free ( Free ( Pure, Free ) )
import Interpreter.Actions ( strToAction )
import Config.App (
      App
    , helpReply
    , repeatReply
    , keyboard
    )

type IOApiLang = IOApi String Int

translate :: App Int () -> BotApi String Int () -> IOApiLang ()
translate _ (Pure x) = Pure x
translate a (Free bf) = free bf where
    free (EchoMessage str f)    = putIOLine str >> translate a f
    free (GetMessages f)        = getIOLine >>= translate a . f . pure
    free (SelectAction str f)   = return (strToAction str (map fst $ keyboard a)) >>= translate a . f 
    free (ShowKeyboard f)       = do
        putIOLine "Available commands:"
        mapM_ (putIOLine . ("/repeat " ++)) $ map snd $ keyboard a 
        translate a f
    free (SetRepeats i f)       = putState i >> translate a f
    free (GetCurrentRepeats f)  = getState >>= translate a . f
    free (TellCurrentRepeats f) = do
        i <- getState
        putIOLine $ repeatReply a ++ ": " ++ show i
        translate a f
    free (ShowHelp f)           = putIOLine (helpReply a) >> translate a f
    free (BotLog m f)           = rawLog m >> translate a f


