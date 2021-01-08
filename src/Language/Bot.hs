{-# LANGUAGE DeriveFunctor #-}

module Language.Bot where

import Log.Message
import Control.Monad.Free

data Action i = Echo | Help | TellRepeat | ModifyRepeat i deriving Show

data BotApiF b i a = GetMessages ([b] -> a)
                   | GetCurrentRepeats (i -> a)
                   | ShowHelp a
                   | EchoMessage b a 
                   | SelectAction b (Action i -> a)
                   | TellCurrentRepeats a
                   | ShowKeyboard a
                   | ApiLog (LogMessage String) a
                   | SetRepeats i a
                   deriving (Functor)

type BotApi b i = Free (BotApiF b i)

echoMessage :: b -> BotApi b i ()
echoMessage b = liftF $ EchoMessage b ()

getMessages :: BotApi b i [b]
getMessages = liftF $ GetMessages id 

selectAction :: b -> BotApi b i (Action i)
selectAction b = liftF $ SelectAction b id

showKeyboard :: BotApi b i ()
showKeyboard = liftF $ ShowKeyboard ()

setRepeats :: i -> BotApi b i ()
setRepeats i = liftF $ SetRepeats i ()

tellCurrentRepeats :: BotApi b i ()
tellCurrentRepeats = liftF $ TellCurrentRepeats ()

getCurrentRepeats :: BotApi b i i
getCurrentRepeats = liftF $ GetCurrentRepeats id

apiLog :: LogMessage String -> BotApi b i ()
apiLog m = liftF $ ApiLog (fmap ("Bot: " ++) m) ()

showHelp :: BotApi b i ()
showHelp = liftF $ ShowHelp ()

