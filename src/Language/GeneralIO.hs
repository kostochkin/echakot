{-# LANGUAGE DeriveFunctor #-}

module Language.GeneralIO where

import Log.Message (LogMessage)
import Control.Monad.Free

data GenIOF req res s a
  = IOMessage req (res -> a)
  | IOState (s -> s) (s -> a)
  | RepeatIO Int (GenIO req res s ()) a
  | LogGenIO (LogMessage String) a
  deriving (Functor)

type GenIO req res s = Free (GenIOF req res s)

runIOMessage :: req -> GenIO req res s res
runIOMessage req = liftF $ IOMessage req id

runIOState :: (s -> s) -> GenIO req res s s
runIOState f = liftF $ IOState f id

putIOState :: s -> GenIO req res s ()
putIOState s = runIOState (const s) >> return ()

getIOState :: GenIO req res s s
getIOState = runIOState id

repeatIO :: Int -> (GenIO req res s ()) -> GenIO req res s ()
repeatIO i m = liftF $ RepeatIO i m ()

logGenIO :: LogMessage String -> GenIO res req s ()
logGenIO m = liftF $ LogGenIO m ()

