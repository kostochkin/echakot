module Interpreter.Test.Config ( interpret ) where

import Language.Config (
      ConfigLangF (
          FailConfig
        , ConfigLog
        , Validate
        , WithDefault
        , Lookup
        , LookupDefault
        , Init
        , LookupString
        )
    , ConfigLang
    , failConfig
    , configLog
    )
import Log.Message (
      LogMessage
    , messageError
    , messageWarn
    ) 
import Control.Monad.Free ( Free (Free, Pure) )
import Text.Read ( readMaybe ) 

--newtype RawString = RawString { getRawString :: String }
--
--instance Show RawString where
--    show = getRawString

interpret :: [(String, String)] -> ConfigLang String [(String, String)] a -> ([LogMessage String], Either String a)
interpret = interpret' False []

interpret' :: Bool -> [LogMessage String] -> [(String, String)] -> ConfigLang String [(String, String)] a -> ([LogMessage String], Either String a)
interpret' _ io _ (Pure x) = (reverse io, Right x)
interpret' n' io kv' (Free f) = free f where
    free (Init _ g) = interpret' n' io kv' (g kv')
    free (LookupString kv k g) = maybe (lookupFail k) (interpret' n' io kv . g) $ Prelude.lookup k kv
    free (Lookup kv k g) = maybe (lookupFail k) readM' $ Prelude.lookup k kv where
        readM' x = maybe (parseFail n' k x) (interpret' n' io kv . g) $ readMaybe x
    free (LookupDefault kv k b g) = maybe (lookupWarn k b g) readM' $ Prelude.lookup k kv where
        readM' x = maybe (parseWarn k x b g) (interpret' n' io kv . g) $ readMaybe x
    free (WithDefault b f' g) = case interpret' True io kv' f' of
        (io', Left _)  -> interpret' n' (reverse io') kv' (g b)
        (io', Right x) -> interpret' n' (reverse io') kv' (g x)
    free (Validate k b g) = case (k b, n') of
        (True, _)  -> interpret' n' io kv' $ g b
        (_, False) -> interpret' n' io kv' $ logAndFail $ "Invalid value: " ++ show b
        (_, True)  -> interpret' n' io kv' $ warnAndFail $ "Invalid value: " ++ show b
    free (ConfigLog s a) = interpret' n' (s : io) kv' a
    free (FailConfig s _) = (reverse io, Left s) 
    lookupFail k = interpret' n' io kv' (logAndFail $ cannotLookup k)
    parseFail True k s = interpret' n' io kv' (warnAndFail $ cannotParse k s)
    parseFail False k s = interpret' n' io kv' (logAndFail $ cannotParse k s)
    lookupWarn k b g = interpret' n' io kv' (warnAndContinue (usingDefault (cannotLookup k) b) b g)
    parseWarn k s b g = interpret' n' io kv' (warnAndContinue (usingDefault (cannotParse k s) b) b g)
    warnAndContinue m b g = configLog (messageWarn m) >> g b
    warnAndFail m = configLog (messageWarn m) >> failConfig m
    logAndFail m = configLog (messageError m) >> failConfig m
    cannotLookup k = "Cannot lookup " ++ show k
    cannotParse k s ="Cannot parse the value of " ++ show k ++ " from string: " ++ show s
    usingDefault s d = s ++ ". Using default " ++ show d
