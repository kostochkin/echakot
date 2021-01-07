module Translator.BotLogger (addLogging) where

import Log
import Language.Bot
import Control.Monad.Free

addLogging :: BotApi String Int () -> BotApi String Int ()
addLogging f@(Pure x) = apiLog (logInfo ("Step finished: " ++ show x)) >> f
addLogging (Free b)   = maybe (free b) debug (tryLog b) where
    debug x = apiLog x >> free b
    free (EchoMessage str g)    = echoMessage str >> addLogging g
    free (GetMessages g)        = getMessages >>= gotValue "messages" >>= addLogging . g
    free (SelectAction str g)   = selectAction str >>= gotValue "action" >>= addLogging . g
    free (ShowKeyboard g)       = showKeyboard >> addLogging g
    free (SetRepeats i g)       = setRepeats i >> addLogging g
    free (GetCurrentRepeats g)  = getCurrentRepeats >>= gotValue "repeats" >>= addLogging . g
    free (TellCurrentRepeats g) = tellCurrentRepeats >> addLogging g
    free (ShowHelp g)           = showHelp >> addLogging g
    free (ApiLog s g)           = apiLog s >> addLogging g
    gotValue s x = do
        apiLog (logDebug ("Got " ++ s ++ ": " ++ show x))
        return x

tryLog :: (Show b, Show i) => BotApiF b i a -> Maybe (LogMessage String)
tryLog (ApiLog _ _)           = Nothing
tryLog (GetMessages _)        = Just $ logInfo "Wait for messages ... "
tryLog (GetCurrentRepeats _ ) = Just $ logDebug "Getting current repeats"
tryLog (ShowHelp _)           = Just $ logDebug "Showing help"
tryLog (EchoMessage b _)      = Just $ logDebug $ "Echoing the message " ++ show b
tryLog (SelectAction b _)     = Just $ logDebug $ "Selecting action for the message " ++ show b
tryLog (TellCurrentRepeats _) = Just $ logDebug "Telling current repeats"
tryLog (ShowKeyboard _)       = Just $ logDebug "Showing the keyboard"
tryLog (SetRepeats i _)       = Just $ logDebug $ "Setting repeats to " ++ show i

