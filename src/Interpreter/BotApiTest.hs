module Interpreter.BotApiTest ( interpret ) where

import Language.Bot (
      BotApi
    , BotApiF ( GetMessages
              , SelectAction
              , ShowHelp
              , TellCurrentRepeats
              , ShowKeyboard
              , SetRepeats
              , GetCurrentRepeats
              , EchoMessage 
              , BotLog
              )
    )
import Control.Monad.Free ( Free( Pure, Free ) )
import Interpreter.Actions ( strToAction )

infixr 9 .:
(.:) :: a -> (b -> [a]) -> b -> [a]
x .: fxs = (x :) . fxs

interpret :: (Read i, Show i, Eq i) => BotApi String i () -> (i, [[String]]) -> [i] -> [String]
interpret (Pure _) _  _ = []
interpret (Free bf) l k = free bf l k where
    free (EchoMessage str f) s          = str .: interpret f s
    free (GetMessages _) (_, [])        = const []
    free (GetMessages f) (i,(x:xs))     = interpret (f x) (i, xs)
    free (SelectAction b f) s           = interpret (f (strToAction b k)) s
    free (ShowKeyboard f) s             = "keyboard" .: interpret f s
    free (SetRepeats i f) (_, xs)       = interpret f (i, xs)
    free (GetCurrentRepeats f) s@(i, _) = interpret (f i) s
    free (TellCurrentRepeats f) s@(i,_) = show i .: interpret f s
    free (ShowHelp f) s                 = "help" .: interpret f s
    free (BotLog _ f) s                 = interpret f s

