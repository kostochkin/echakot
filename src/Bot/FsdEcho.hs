module Bot.FsdEcho (botStep) where

import Bot.Api
import Control.Monad

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

