module Bot.FsdEcho (
        botStep
    ) where

import Language.Bot (
      Action(Help, TellRepeat, ModifyRepeat, Echo)
    , BotApi
    , getMessages
    , selectAction
    , showHelp
    , tellCurrentRepeats
    , showKeyboard
    , setRepeats
    , getCurrentRepeats
    , echoMessage 
    )
import Control.Monad ( forM_ )

botStep :: (Integral i) => BotApi b i ()
botStep = do
    ms <- getMessages
    mapM_ procMessage ms
    return ()

procMessage :: (Integral i) => b -> BotApi b i ()
procMessage m = do
    a <- selectAction m
    botAction a m

botAction :: (Integral i) => Action i -> b -> BotApi b i ()
botAction Help             = const $ showHelp
botAction TellRepeat       = const $ tellCurrentRepeats >> showKeyboard
botAction (ModifyRepeat i) = const $ setRepeats i
botAction Echo             = \m -> do
    i <- getCurrentRepeats
    forM_ (replicate (fromIntegral i) m) echoMessage
    return ()

