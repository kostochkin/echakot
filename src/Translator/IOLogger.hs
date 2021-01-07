module Translator.IOLogger where

import Log
import Language.IO
import Control.Monad.Free

addLogging :: IOApi String Int () -> IOApi String Int ()
addLogging f@(Pure x) = ioLog logDebug ("IO finished: " ++ show x) >> f
addLogging (Free b)   = maybe (free b) logIt (tryLog b) where
    logIt x = x >> free b
    free (GetIOLine g)   = logValue getIOLine logInfo "line" >>= addLogging . g
    free (PutIOLine s g) = putIOLine s >> addLogging g
    free (ReturnIO io g) = logValue (returnIO io) logDebug "value" >>= addLogging . g
    free (RunIO io g)    = runIO io >> addLogging g
    free (RawLog s g)    = rawLog s >> addLogging g


logValue :: (Show a) => IOApi String Int a -> (String -> LogMessage String) -> String -> IOApi String Int a
logValue m f s = do
    x <- m
    ioLog f ("Got " ++ s ++ ": " ++ show x)
    return x

tryLog :: (Show b, Show i) => IOApiF b i a -> Maybe (IOApi b i ())
tryLog (RawLog _ _)    = Nothing
tryLog (GetIOLine _)   = Just $ ioLog logDebug "Getting line ... "
tryLog (PutIOLine b _) = Just $ ioLog logInfo $ "Writing line " ++ show b
tryLog (RunIO _ _)     = Just $ ioLog logDebug "Running raw IO ()"
tryLog (ReturnIO _ _)  = Just $ ioLog logDebug "Querying raw IO a"

