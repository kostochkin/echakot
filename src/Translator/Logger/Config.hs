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
addLogging = addLogging' False

addLogging' :: (Show k) => Bool -> Logger (ConfigLang k c) String () -> ConfigLang k c a -> ConfigLang k c a
addLogging' _ _ f@(Pure _) = f
addLogging' e l (Free f) = maybe (free f) (\m -> m |> l *>> free f) $ maybeMessage e f where
    free (Init s g)              = init s >>= addLogging' e l . g
    free (LookupString c k g)    = lookupString c k >>=* ls |-> l *>>= addLogging' e l . g
    free (Lookup c k g)          = lookup c k >>=* lss |-> l *>>= addLogging' e l . g
    free (LookupDefault c k b g) = lookupDefault c k b >>=* lss |-> l *>>= addLogging' e l . g
    free (WithDefault d f' g)    = withDefault d (addLogging' True l f') >>=* lss |-> l *>>= addLogging' e l . g
    free (Validate k b g)        = validate k b >>=* inlineLogDebugV (const "Validated") l *>>= addLogging' e l . g
    free (FailConfig s g)        = failConfig s >>= addLogging' e l . g
    free (ConfigLog s g)         = configLog s >> addLogging' e l g
    ls = messageDebug $ ("Got: " ++)
    lss :: (Show a) => LogMessage (a -> String)
    lss = messageDebug $ ("Got: " ++) . show


maybeMessage :: (Show k) => Bool -> ConfigLangF k c a -> Maybe (LogMessage String)
maybeMessage _ (Init s _)              = Just $ messageInfo $ "Reading config: " ++ s
maybeMessage _ (LookupString _ k _)    = Just $ messageDebug $ "Seeking string " ++ show k
maybeMessage _ (Lookup _ k _)          = Just $ messageDebug $ "Seeking " ++ show k
maybeMessage _ (LookupDefault _ k b _) = Just $ messageDebug $ "Seeking " ++ show k ++ " (default = " ++ show b ++ ")"
maybeMessage _ (WithDefault d _ _)     = Just $ messageDebug $ "Using default: " ++ show d
maybeMessage _ (Validate _ b _ )       = Just $ messageDebug $ "Validating: " ++ show b
maybeMessage False (FailConfig s _ )   = Just $ messageError $ "Fail: " ++ show s
maybeMessage True (FailConfig s _ )    = Just $ messageWarn $ "Fail: " ++ show s
maybeMessage _ (ConfigLog _ _)         = Nothing
