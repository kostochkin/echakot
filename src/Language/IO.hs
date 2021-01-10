{-# LANGUAGE DeriveFunctor #-}

module Language.IO (
              IOApi
            , IOApiF (
                  GetIOLine
                , PutIOLine
                , GetState
                , PutState
                , ReturnIO
                , RunIO
                , RawLog
                )
            , getIOLine
            , putIOLine
            , getState
            , putState
            , runIO
            , returnIO
            , rawLog
            , ioLog
    ) where

--import Prelude hiding (log)
import Log.Message ( LogMessage )
import Control.Monad.Free ( Free, liftF )

data IOApiF b s a = GetIOLine (b -> a)
                  | PutIOLine b a
                  | GetState (s -> a)
                  | PutState s a
                  | ReturnIO (IO s) (s -> a)
                  | RunIO (IO ()) a
                  | RawLog (LogMessage String) a
                  deriving (Functor)


type IOApi b s = Free (IOApiF b s)


getIOLine :: IOApi b s b
getIOLine = liftF $ GetIOLine id

putIOLine :: b -> IOApi b s ()
putIOLine b = liftF $ PutIOLine b ()

ioLog :: LogMessage String -> IOApi b s ()
ioLog m = liftF $ RawLog (("IO: " ++) <$> m) ()

returnIO :: IO s -> IOApi b s s
returnIO io = liftF $ ReturnIO io id

runIO :: IO () -> IOApi b s ()
runIO io = liftF $ RunIO io ()

rawLog :: LogMessage String -> IOApi b s ()
rawLog s = liftF $ RawLog s ()

getState :: IOApi b s s 
getState = liftF $ GetState id

putState :: s -> IOApi b s ()
putState s = liftF $ PutState s ()

