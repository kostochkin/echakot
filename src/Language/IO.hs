{-# LANGUAGE DeriveFunctor #-}

module Language.IO where

import Log
import Control.Monad.Free

data IOApiF b i a = GetIOLine (b -> a)
                  | PutIOLine b a
                  | ReturnIO (IO i) (i -> a)
                  | RunIO (IO ()) a
                  | RawLog (LogMessage String) a
                  deriving (Functor)

type IOApi b i = Free (IOApiF b i)

getIOLine :: IOApi b i b
getIOLine = liftF $ GetIOLine id

putIOLine :: b -> IOApi b i ()
putIOLine b = liftF $ PutIOLine b ()

ioLog :: (String -> LogMessage String) -> String -> IOApi b i ()
ioLog f s = liftF $ RawLog (f ("IO: " ++ s)) ()

returnIO :: IO i -> IOApi b i i
returnIO io = liftF $ ReturnIO io id

runIO :: IO () -> IOApi b i ()
runIO io = liftF $ RunIO io ()

rawLog :: LogMessage String -> IOApi b i ()
rawLog s = liftF $ RawLog s ()

