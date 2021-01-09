module Translator.IO (
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
    , runIO
    , returnIO
    , rawLog
    )    

import Control.Monad.Free ( Free ( Pure, Free ) )
import Interpreter.Actions ( strToAction )
import Messenger.IO (
      IOMessenger
    , messengerRepeats
    , modifyRepeats
    , helpString
    , keyboardLabels
    , keyboardValues
    )

type IOApiLang = IOApi String Int

translate :: IOMessenger a => a -> BotApi String Int () -> IOApiLang ()
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
    free (BotLog m f)           = rawLog m >> translate a f


