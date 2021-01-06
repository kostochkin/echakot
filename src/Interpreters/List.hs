module Interpreters.List where

import Bot.Api
import Control.Monad.Free
import Interpreters.Actions

interpret :: (Read i, Show i, Eq i) => BotApi String i () -> (i, [String]) -> [i] -> [String]
interpret (Pure x) xs k = []
interpret (Free f) xs k = free f xs where
    free a@(EchoMessage str f) s           = str : interpret f s k
    free (GetMessage _) (_, [])            = []
    free a@(GetMessage f) (i,(x:xs))       = interpret (f x) (i, xs) k
    free a@(SelectAction b f) s            = interpret (f (strToAction b k)) s k
    free a@(ShowKeyboard f) s              = "keyboard" : interpret f s k
    free a@(SetRepeats i f) (_, xs)        = interpret f (i, xs) k
    free a@(GetCurrentRepeats f) s@(i, xs) = interpret (f i) s k
    free a@(TellCurrentRepeats f) s@(i,_)  = show i : interpret f s k
    free a@(ShowHelp f) s                  = "help" : interpret f s k
    free a@(ApiLog _ _ f) s              = interpret f s k
