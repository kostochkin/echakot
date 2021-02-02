module Translator.Logger.Bot
  ( addLogging
  , defaultLogger
  , addDefaultLogging
  ) where

import Control.Monad.Free (Free(Free, Pure))
import Language.Bot
  ( BotApi
  , BotApiF(..)
  , botLog
  , echoMessage
  , getCurrentRepeats
  , getMessages
  , runTimes
  , selectAction
  , setRepeats
  , showHelp
  , showKeyboard
  , tellCurrentRepeats
  )
import Log.Logger
  ( Logger
  , (*>>)
  , (*>>=)
  , (>>=*)
  , (|>)
  , inlineLogDebugV
  , inlineLogInfoV
  , loggerFromString
  )
import Log.Message (LogMessage, messageDebug, messageInfo)

type BotLang = BotApi String Int

defaultLogger ::
     Maybe (LogMessage String -> BotLang ())
  -> String
  -> Logger BotLang String ()
defaultLogger m s = loggerFromString (maybe botLog id m) s

addDefaultLogging :: String -> BotLang () -> BotLang ()
addDefaultLogging = addLogging . defaultLogger Nothing

addLogging :: Logger BotLang String () -> BotLang () -> BotLang ()
addLogging _ f@(Pure _) = f
addLogging l (Free b) =
  maybe (free b) (\m -> m |> l *>> free b) $ maybeMessage b
  where
    free (EchoMessage str g) = echoMessage str >> addLogging l g
    free (GetMessages g) =
      getMessages >>=* inlineLogInfoV (show' . length) l *>>= addLogging l . g
    free (SelectAction str g) =
      selectAction str >>=* inlineLogDebugV (got "action") l *>>= addLogging l .
      g
    free (ShowKeyboard g) = showKeyboard >> addLogging l g
    free (SetRepeats i g) = setRepeats i >> addLogging l g
    free (GetCurrentRepeats g) =
      getCurrentRepeats >>=* inlineLogDebugV (got "repeats") l *>>= addLogging l .
      g
    free (TellCurrentRepeats g) = tellCurrentRepeats >> addLogging l g
    free (ShowHelp g) = showHelp >> addLogging l g
    free (BotLog s g) = botLog s >> addLogging l g
    free (RunTimes i p g) = runTimes i (addLogging l p) >> addLogging l g
    got w x = "Got " ++ w ++ ": " ++ show x
    show' i =
      "Got " ++ show i ++ " message" ++
      (if i == 1
         then ""
         else "s")

maybeMessage :: (Show b, Show i) => BotApiF b i a -> Maybe (LogMessage String)
maybeMessage (BotLog _ _) = Nothing
maybeMessage (GetMessages _) = Just $ messageInfo "Wait for messages ... "
maybeMessage (GetCurrentRepeats _) =
  Just $ messageDebug "Getting current repeats"
maybeMessage (ShowHelp _) = Just $ messageDebug "Showing help"
maybeMessage (EchoMessage b _) =
  Just $ messageDebug $ "Echoing the message " ++ show b
maybeMessage (SelectAction b _) =
  Just $ messageDebug $ "Selecting action for the message " ++ show b
maybeMessage (TellCurrentRepeats _) =
  Just $ messageDebug "Telling current repeats"
maybeMessage (ShowKeyboard _) = Just $ messageDebug "Showing the keyboard"
maybeMessage (SetRepeats i _) =
  Just $ messageDebug $ "Setting repeats to " ++ show i
maybeMessage (RunTimes i _ _) =
  Just $ messageDebug $ "Running " ++ show i ++ " times"
