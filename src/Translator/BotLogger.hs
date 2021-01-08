module Translator.BotLogger (addLogging) where

import qualified Log.Message as LM
import qualified Log.Logger as LL
import Language.Bot
import Control.Monad.Free
import Control.Monad

addLogging :: LL.Logger -> BotApi String Int () -> BotApi String Int ()
addLogging l f@(Pure x) = LL.maybeLog l f (apiLog >=> const f) $ LM.logInfo ("Step finished: " ++ show x)
addLogging l (Free b)   = maybe (free b) (apiLog >=> const (free b)) (LL.maybeMessage l tryMessage b) where
    free (EchoMessage str g)    = echoMessage str >> addLogging l g
    free (GetMessages g)        = maybeLogValue getMessages (LM.logDebug "messages") >>= addLogging l . g
    free (SelectAction str g)   = maybeLogValue (selectAction str) (LM.logDebug "action") >>= addLogging l . g
    free (ShowKeyboard g)       = showKeyboard >> addLogging l g
    free (SetRepeats i g)       = setRepeats i >> addLogging l g
    free (GetCurrentRepeats g)  = maybeLogValue getCurrentRepeats (LM.logDebug "repeats") >>= addLogging l . g
    free (TellCurrentRepeats g) = tellCurrentRepeats >> addLogging l g
    free (ShowHelp g)           = showHelp >> addLogging l g
    free (ApiLog s g)           = apiLog s >> addLogging l g
    maybeLogValue f m = LL.maybeLogValue l f (fmap (\x y -> ("Got " ++ x ++ ": " ++ show y)) m) apiLog

tryMessage :: (Show b, Show i) => BotApiF b i a -> Maybe (LM.LogMessage String)
tryMessage (ApiLog _ _)           = Nothing
tryMessage (GetMessages _)        = Just $ LM.logInfo "Wait for messages ... "
tryMessage (GetCurrentRepeats _ ) = Just $ LM.logDebug "Getting current repeats"
tryMessage (ShowHelp _)           = Just $ LM.logDebug "Showing help"
tryMessage (EchoMessage b _)      = Just $ LM.logDebug $ "Echoing the message " ++ show b
tryMessage (SelectAction b _)     = Just $ LM.logDebug $ "Selecting action for the message " ++ show b
tryMessage (TellCurrentRepeats _) = Just $ LM.logDebug "Telling current repeats"
tryMessage (ShowKeyboard _)       = Just $ LM.logDebug "Showing the keyboard"
tryMessage (SetRepeats i _)       = Just $ LM.logDebug $ "Setting repeats to " ++ show i

