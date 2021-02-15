{-# LANGUAGE DeriveFunctor #-}

module Language.IO
  ( IOApi
  , IOApiF(GetIOLine, PutIOLine, GetState, PutState,
       RawLog, Replicate)
  , getIOLine
  , putIOLine
  , getState
  , putState
  , rawLog
  , ioLog
  , replicate
  ) where

import Control.Monad.Free (Free, liftF)
import Log.Message (LogMessage)
import Prelude hiding (replicate)

data IOApiF b s a
  = GetIOLine (b -> a)
  | PutIOLine b a
  | GetState (s -> a)
  | PutState s a
  | Replicate Int (IOApi b s ()) a
  | RawLog (LogMessage String) a
  deriving (Functor)

type IOApi b s = Free (IOApiF b s)

getIOLine :: IOApi b s b
getIOLine = liftF $ GetIOLine id

putIOLine :: b -> IOApi b s ()
putIOLine b = liftF $ PutIOLine b ()

ioLog :: LogMessage String -> IOApi b s ()
ioLog m = liftF $ RawLog (("IO: " ++) <$> m) ()

rawLog :: LogMessage String -> IOApi b s ()
rawLog s = liftF $ RawLog s ()

getState :: IOApi b s s
getState = liftF $ GetState id

putState :: s -> IOApi b s ()
putState s = liftF $ PutState s ()

replicate :: Int -> IOApi b s () -> IOApi b s ()
replicate i p = liftF $ Replicate i p ()
