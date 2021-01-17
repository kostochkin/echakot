module Interpreter.IO (
      interpret
    , defaultLogger
     ) where

import Language.IO (
          IOApi
        , IOApiF ( .. )
        )
import Control.Monad ( replicateM_ )
import Control.Monad.Free ( Free( Pure, Free ) )
import Log.Message ( LogMessage)
import Log.Logger (
      Logger
    , loggerFromString
    , runLogger
    )
import Control.Concurrent.MVar (
      readMVar
    , takeMVar
    , putMVar
    )
import Interpreter.Actions (
      TimeTagged
    , toTimeTagged
    )
import Config.Messenger.StdIO (
      StdioMessenger
    , state
    )
import System.IO (
      hFlush
    , stdout
    )

defaultLogger :: (Show a) => Maybe (LogMessage a -> IO ()) -> String -> Logger IO a ()
defaultLogger m s = loggerFromString (maybe print id m) s

interpret :: StdioMessenger -> Logger IO TimeTagged () -> IOApi String Int () -> IO ()
interpret _ _ (Pure _)     = return ()
interpret s l (Free bf)    = free bf where
    free (GetIOLine f)     = putStr ">> " >> hFlush stdout >> getLine >>= interpret s l . f
    free (PutIOLine str f) = putStrLn str >> interpret s l f
    free (GetState f)      = readMVar (state s) >>= interpret s l . f
    free (PutState ns f)   = takeMVar (state s) >> putMVar (state s)  ns >> interpret s l f
    free (ReturnIO io f)   = io >>= interpret s l . f
    free (RunIO io f)      = io >> interpret s l f
    free (RawLog m f)      = do
        tt <- toTimeTagged
        runLogger l (fmap tt m) >> interpret s l f
    free (Replicate i p f) = replicateM_ i (interpret s l p) >> interpret s l f

