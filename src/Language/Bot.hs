{-# LANGUAGE DeriveFunctor #-}

module Language.Bot where

import Log
import Control.Monad.Free

data Action i = Echo | Help | TellRepeat | ModifyRepeat i deriving Show

data BotApiF b i a = GetMessages ([b] -> a)
                   | GetCurrentRepeats (i -> a)
                   | ShowHelp a
                   | EchoMessage b a 
                   | SelectAction b (Action i -> a)
                   | TellCurrentRepeats a
                   | ShowKeyboard a
                   | ApiLog LogLevel String a
                   | SetRepeats i a
                   deriving (Functor)

type BotApi b i = Free (BotApiF b i)

instance (Show b, Show i) => Loggable (BotApiF b i a) where
    tryLog (ApiLog _ _ _) = Nothing
    tryLog f              = Just $ fmtApi f

fmtApi :: (Show b, Show i) => BotApiF b i a -> String
fmtApi (GetMessages _)        = "Wait for messages ... "
fmtApi (GetCurrentRepeats _ ) = "Getting current repeats"
fmtApi (ShowHelp _)           = "Showing help"
fmtApi (EchoMessage b _)      = "Echoing the message " ++ show b
fmtApi (SelectAction b _)     = "Selecting action for the message " ++ show b
fmtApi (TellCurrentRepeats _) = "Telling current repeats"
fmtApi (ShowKeyboard _)       = "Showing the keyboard"
fmtApi (SetRepeats i _)       = "Setting repeats to " ++ show i
fmtApi (ApiLog _ _ _)         = ""

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

apiLog :: LogLevel -> String -> BotApi b i ()
apiLog l m = liftF $ ApiLog l ("[Bot] " ++ m) ()

showHelp :: BotApi b i ()
showHelp = liftF $ ShowHelp ()

