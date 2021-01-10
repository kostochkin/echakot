module Interpreter.IO (
      interpret
    , defaultLogger
    , toRawString
     ) where

import Language.IO (
          IOApi
        , IOApiF ( .. )
        )
import Control.Monad.Free ( Free( Pure, Free ) )
import Log.Message ( LogMessage)
import Log.Logger (
      Logger
    , loggerFromString
    , runLogger
    )
import Control.Concurrent.MVar (
      MVar
    , readMVar
    , takeMVar
    , putMVar
    )

newtype RawString = RawString { getString :: String }

instance Show RawString where
    show = getString

toRawString :: LogMessage String -> LogMessage RawString
toRawString = fmap RawString

defaultLogger :: (Show a) => Maybe (LogMessage a -> IO ()) -> String -> Logger IO a ()
defaultLogger m s = loggerFromString (maybe print id m) s

interpret :: MVar Int -> Logger IO RawString () -> IOApi String Int () -> IO ()
interpret _ _ (Pure _)     = return ()
interpret s l (Free bf)    = free bf where
    free (GetIOLine f)     = getLine >>= interpret s l . f
    free (PutIOLine str f) = putStrLn str >> interpret s l f
    free (GetState f)      = readMVar s >>= interpret s l . f
    free (PutState ns f)   = takeMVar s >> putMVar s ns >> interpret s l f
    free (ReturnIO io f)   = io >>= interpret s l . f
    free (RunIO io f)      = io >> interpret s l f
    free (RawLog m f)      = runLogger l (fmap RawString m) >> interpret s l f

