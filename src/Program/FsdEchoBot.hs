module Program.FsdEchoBot
  ( botStep
  ) where

import Language.Bot
  ( Action(Echo, Help, ModifyRepeat, TellRepeat)
  , BotApi
  , echoMessage
  , getCurrentRepeats
  , getMessages
  , runTimes
  , selectAction
  , setRepeats
  , showHelp
  , showKeyboard
  , tellCurrentRepeats
  )

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
botAction Help = const $ showHelp
botAction TellRepeat = const $ tellCurrentRepeats >> showKeyboard
botAction (ModifyRepeat i) = const $ setRepeats i
botAction Echo =
  \m -> do
    i <- getCurrentRepeats
    runTimes (fromIntegral i) (echoMessage m)
