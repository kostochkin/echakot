module Translator.IOLogger (
      addLogging
    , addDefaultLogging
    , defaultLogger
    ) where

import Log.Message (
      LogMessage
    , messageInfo
    , messageDebug
    )
import Translator.IO ( IOApiLang )
import Log.Logger (
      Logger
    , loggerFromString
    , inlineLogDebug
    , inlineLogInfoV
    , inlineLogDebugV
    , (*>>=)
    , (>>=*)
    , (*>>)
    , (|>)
    )
import Language.IO (
      IOApiF(..)
    , getIOLine
    , putIOLine
    , runIO
    , returnIO
    , rawLog
    , ioLog
    )
import Control.Monad.Free ( Free ( Pure, Free ) )

defaultLogger :: Maybe (LogMessage String -> IOApiLang ()) -> String -> Logger IOApiLang String ()
defaultLogger m s = loggerFromString (maybe ioLog id m) s

addDefaultLogging :: String -> IOApiLang () -> IOApiLang ()
addDefaultLogging = addLogging . defaultLogger Nothing

addLogging :: Logger IOApiLang String () -> IOApiLang () -> IOApiLang ()
addLogging l f@(Pure x) = inlineLogDebug ("IO finished: " ++ show x) l *>> f
addLogging l (Free b)   = maybe (free b) (\m -> m |> l *>> free b) $ maybeMessage b where
    free (GetIOLine g)   = getIOLine >>=* inlineLogInfoV gotLine l *>>= addLogging l . g
    free (PutIOLine s g) = putIOLine s >> addLogging l g
    free (ReturnIO io g) = returnIO io >>=* inlineLogDebugV returned l *>>= addLogging l . g
    free (RunIO io g)    = runIO io >> addLogging l g
    free (RawLog s g)    = rawLog s >> addLogging l g
    gotLine y = "Got line: " ++ show y
    returned y = "Returned value: " ++ show y

maybeMessage :: (Show b, Show i) => IOApiF b i a -> Maybe (LogMessage String)
maybeMessage (RawLog _ _)    = Nothing
maybeMessage (GetIOLine _)   = Just $ messageDebug "Getting line ... "
maybeMessage (PutIOLine b _) = Just $ messageInfo $ "Writing line " ++ show b
maybeMessage (RunIO _ _)     = Just $ messageDebug "Running raw IO"
maybeMessage (ReturnIO _ _)  = Just $ messageDebug "Querying raw IO"

