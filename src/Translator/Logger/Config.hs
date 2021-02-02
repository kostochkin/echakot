module Translator.Logger.Config where

import Control.Monad.Free
import Language.Config
import Log.Logger
import Log.Message
import Prelude hiding (init, lookup)

defaultLogger :: Maybe (LogMessage String -> ConfigLang c ()) -> String -> Logger (ConfigLang c) String ()
defaultLogger m s = loggerFromString (maybe configLog id m) s

addDefaultLogging :: String -> ConfigLang c a -> ConfigLang c a
addDefaultLogging = addLogging . defaultLogger Nothing

addLogging :: Logger (ConfigLang c) String () -> ConfigLang c a -> ConfigLang c a
addLogging = addLogging' False

addLogging' :: Bool -> Logger (ConfigLang c) String () -> ConfigLang c a -> ConfigLang c a
addLogging' _ _ f@(Pure _) = f
addLogging' e l (Free f) =
  maybe (free f) (\m -> m |> l *>> free f) $ maybeMessage e f
  where
    free (Init s g) = init s >>= addLogging' e l . g
    free (Lookup c k g) = lookup c k >>=* lss |-> l *>>= addLogging' e l . g
    free (WithDefault d f' g) =
      withDefault d (addLogging' True l f') >>=* lss |-> l *>>= addLogging' e l . g
    free (Validate k b g) =
      validate k b >>=* inlineLogDebugV (const "Validated") l *>>=
      addLogging' e l . g
    free (FailConfig s g) = failConfig s >>= addLogging' e l . g
    free (ConfigLog s g) = configLog s >> addLogging' e l g
    lss :: (Show a) => LogMessage (a -> String)
    lss = messageDebug $ ("Got: " ++) . show

maybeMessage :: Bool -> ConfigLangF c a -> Maybe (LogMessage String)
maybeMessage _ (Init s _) = Just $ messageInfo $ "Reading config: " ++ s
maybeMessage _ (Lookup _ k _) = Just $ messageDebug $ "Seeking " ++ show k
maybeMessage _ (WithDefault d _ _) =
  Just $ messageDebug $ "Using default: " ++ show d
maybeMessage _ (Validate _ b _) = Just $ messageDebug $ "Validating: " ++ show b
maybeMessage False (FailConfig s _) = Just $ messageError $ "Fail: " ++ show s
maybeMessage True (FailConfig s _) = Just $ messageWarn $ "Fail: " ++ show s
maybeMessage _ (ConfigLog _ _) = Nothing
