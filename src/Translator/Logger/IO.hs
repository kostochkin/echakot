module Translator.Logger.IO (
      addLogging
    , addDefaultLogging
    , defaultLogger
    ) where

import Prelude hiding (replicate)
import Log.Message (
      LogMessage
    , messageInfo
    , messageDebug
    )
import Translator.Messenger.StdIO ( IOApiLang )
import Log.Logger (
      Logger
    , loggerFromString
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
    , getState
    , putState
    , runIO
    , returnIO
    , rawLog
    , ioLog
    , replicate
    )
import Control.Monad.Free ( Free ( Pure, Free ) )


defaultLogger :: Maybe (LogMessage String -> IOApiLang ()) -> String -> Logger IOApiLang String ()
defaultLogger m s = loggerFromString (maybe ioLog id m) s

addDefaultLogging :: String -> IOApiLang () -> IOApiLang ()
addDefaultLogging = addLogging . defaultLogger Nothing

addLogging :: Logger IOApiLang String () -> IOApiLang () -> IOApiLang ()
addLogging _ f@(Pure _) = f
addLogging l (Free b)   = maybe (free b) (\m -> m |> l *>> free b) $ maybeMessage b where
    free (GetIOLine g)     = getIOLine >>=* inlineLogInfoV gotLine l *>>= addLogging l . g
    free (PutIOLine s g)   = putIOLine s >> addLogging l g
    free (GetState g)      = getState >>=* inlineLogDebugV gotState l *>>= addLogging l . g
    free (PutState s g)    = putState s >> addLogging l g
    free (ReturnIO io g)   = returnIO io >>=* inlineLogDebugV returned l *>>= addLogging l . g
    free (RunIO io g)      = runIO io >> addLogging l g
    free (RawLog s g)      = rawLog s >> addLogging l g
    free (Replicate i p g) = replicate i (addLogging l p) >> addLogging l g
    gotLine y = "Got line: " ++ show y
    gotState s = "State: " ++ show s
    returned y = "Returned value: " ++ show y

maybeMessage :: (Show b, Show i) => IOApiF b i a -> Maybe (LogMessage String)
maybeMessage (RawLog _ _)       = Nothing
maybeMessage (GetIOLine _)      = Just $ messageDebug "Getting line ... "
maybeMessage (PutIOLine b _)    = Just $ messageInfo $ "Writing line " ++ show b
maybeMessage (GetState _)       = Just $ messageDebug $ "Getting state"
maybeMessage (PutState s _)     = Just $ messageDebug $ "Updating state: " ++ show s
maybeMessage (RunIO _ _)        = Just $ messageDebug "Running raw IO"
maybeMessage (ReturnIO _ _)     = Just $ messageDebug "Querying raw IO"
maybeMessage (Replicate i _ _)  = Just $ messageDebug $ "Replicate " ++ show i ++ " times"

