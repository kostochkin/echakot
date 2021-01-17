{-# LANGUAGE DeriveFunctor #-}

module Language.Bot (
          Action (Echo, Help, TellRepeat, ModifyRepeat)
        , BotApi
        , BotApiF (
                  EchoMessage
                , GetMessages
                , SelectAction
                , ShowKeyboard
                , SetRepeats
                , RunTimes
                , TellCurrentRepeats
                , GetCurrentRepeats
                , BotLog
                , ShowHelp
            )
        , echoMessage
        , getMessages
        , selectAction
        , showKeyboard
        , setRepeats
        , runTimes
        , tellCurrentRepeats
        , getCurrentRepeats
        , botLog
        , showHelp
        ) where

import Log.Message ( LogMessage )
import Control.Monad.Free ( Free, liftF )

data Action i = Echo | Help | TellRepeat | ModifyRepeat i deriving Show

data BotApiF b i a = GetMessages ([b] -> a)
                   | GetCurrentRepeats (i -> a)
                   | ShowHelp a
                   | EchoMessage b a 
                   | SelectAction b (Action i -> a)
                   | TellCurrentRepeats a
                   | ShowKeyboard a
                   | BotLog (LogMessage String) a
                   | SetRepeats i a
                   | RunTimes i (BotApi b i ()) a
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

runTimes :: i -> BotApi b i () -> BotApi b i ()
runTimes i p = liftF $ RunTimes i p ()

tellCurrentRepeats :: BotApi b i ()
tellCurrentRepeats = liftF $ TellCurrentRepeats ()

getCurrentRepeats :: BotApi b i i
getCurrentRepeats = liftF $ GetCurrentRepeats id

botLog :: LogMessage String -> BotApi b i ()
botLog m = liftF $ BotLog (fmap ("Bot: " ++) m) ()

showHelp :: BotApi b i ()
showHelp = liftF $ ShowHelp ()

