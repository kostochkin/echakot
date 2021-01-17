module Translator.Logger.Config where

import Language.Config
import Prelude hiding ( init, lookup )
import Control.Monad.Free
import Log.Logger
import Log.Message

defaultLogger :: Maybe (LogMessage String -> ConfigLang k c ()) -> String -> Logger (ConfigLang k c) String ()
defaultLogger m s = loggerFromString (maybe configLog id m) s

addDefaultLogging :: (Show k) => String -> ConfigLang k c a -> ConfigLang k c a
addDefaultLogging = addLogging . defaultLogger Nothing

addLogging :: (Show k) => Logger (ConfigLang k c) String () -> ConfigLang k c a -> ConfigLang k c a
addLogging _ f@(Pure _) = f
addLogging l (Free f) = maybe (free f) (\m -> m |> l *>> free f) $ maybeMessage f where
    free (Init s g)              = init s >>= addLogging l . g
    free (LookupString c k g)    = lookupString c k >>=* ls |-> l *>>= addLogging l . g
    free (Lookup c k g)          = lookup c k >>=* lss |-> l *>>= addLogging l . g
    free (LookupDefault c k b g) = lookupDefault c k b >>=* lss |-> l *>>= addLogging l . g
    free (FailConfig s g)        = failConfig s >>= addLogging l . g
    free (ConfigLog s g)         = configLog s >> addLogging l g
    ls = messageDebug $ ("Got: " ++)
    lss :: (Show a) => LogMessage (a -> String)
    lss = messageDebug $ ("Got: " ++) . show


maybeMessage :: (Show k) => ConfigLangF k c a -> Maybe (LogMessage String)
maybeMessage (Init s _)              = Just $ messageInfo $ "Reading config: " ++ s
maybeMessage (LookupString _ k _)    = Just $ messageDebug $ "Seeking string " ++ show k
maybeMessage (Lookup _ k _)          = Just $ messageDebug $ "Seeking " ++ show k
maybeMessage (LookupDefault _ k b _) = Just $ messageDebug $ "Seeking " ++ show k ++ " (default = " ++ show b ++ ")"
maybeMessage (FailConfig s _ )       = Just $ messageError $ "Fail: " ++ show s
maybeMessage (ConfigLog _ _)         = Nothing
