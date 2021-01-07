module Bot.FsdEcho (botStep) where

import Language.Bot
import Control.Monad

botStep :: BotApi b Int ()
botStep = do
    ms <- getMessages
    mapM_ procMessage ms
    return ()

procMessage :: b -> BotApi b Int ()
procMessage m = do
    a <- selectAction m
    botAction a m


botAction :: Action Int -> b -> BotApi b Int ()
botAction Help             = const $ showHelp
botAction TellRepeat       = const $ tellCurrentRepeats >> showKeyboard
botAction (ModifyRepeat i) = const $ setRepeats i
botAction Echo             = \m -> getCurrentRepeats >>= flip replicateM (echoMessage m) >> return ()

