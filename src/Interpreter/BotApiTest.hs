module Interpreter.BotApiTest ( interpret ) where

import Language.Bot (
      BotApi
    , BotApiF ( GetMessages
              , SelectAction
              , ShowHelp
              , TellCurrentRepeats
              , ShowKeyboard
              , SetRepeats
              , RunTimes
              , GetCurrentRepeats
              , EchoMessage 
              , BotLog
              )
    )
import Control.Monad.Free ( Free( Pure, Free ) )
import Interpreter.Actions ( strToAction )

--infixr 9 .:
--(.:) :: a -> (b -> [a]) -> b -> [a]
--x .: fxs = (x :) . fxs

type S i = (i, [[String]])
type Result i = (S i, [String])

interpret :: (Read i, Show i, Eq i, Ord i, Num i) => BotApi String i () -> S i -> [i] -> [String]
interpret f s k = reverse $ snd $ interpret' f s k []

interpret' :: (Read i, Show i, Eq i, Ord i, Num i) => BotApi String i () -> S i -> [i] -> [String] -> Result i
interpret' (Pure _) s0 _ acc0  = (s0, acc0)
interpret' (Free bf) s0 k0 acc0 = free bf s0 k0 acc0 where
    free (EchoMessage str f) s k acc          = interpret' f s k (str : acc)
    free (GetMessages _) s@(_, []) _ acc      = (s, acc)
    free (GetMessages f) (i,(x:xs)) k acc     = interpret' (f x) (i, xs) k acc
    free (SelectAction b f) s k acc           = interpret' (f (strToAction b k)) s k acc
    free (ShowKeyboard f) s k acc             = interpret' f s k ("keyboard":acc)
    free (SetRepeats i f) (_, xs) k acc       = interpret' f (i, xs) k acc
    free (GetCurrentRepeats f) s@(i, _) k acc = interpret' (f i) s k acc
    free (TellCurrentRepeats f) s@(i,_) k acc = interpret' f s k (show i : acc)
    free (RunTimes i p f) s k acc             = interpret'' i s acc where
        interpret'' i' s1 acc1
            | i' < 1     = interpret' f s1 k acc1
            | otherwise = let (s1', acc1') = interpret' p s k acc1 in interpret'' (i' - 1) s1' acc1'
    free (ShowHelp f) s k acc                 = interpret' f s k ("help":acc)
    free (BotLog _ f) s k acc                 = interpret' f s k acc



