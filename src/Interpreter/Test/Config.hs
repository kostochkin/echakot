{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Test.Config
  ( interpret
  ) where

import Control.Monad.Free (Free(Free, Pure))
import Language.Config
  ( ConfigLang
  , ConfigLangF(ConfigLog, FailConfig, Init, Lookup, Validate, WithDefault)
  , configLog
  , failConfig
  , Parsable
  , from
  , Semiredis (Key, Val)
  , dbLookup
  )
import Log.Message (LogMessage, messageError, messageWarn)
import Text.Read (readMaybe)

newtype LTS = LTS [(String, String)]

instance Show LTS where
    show (LTS x) = show x

instance Semiredis LTS where
    type Key LTS = String
    type Val LTS = String
    dbLookup (LTS x) = flip lookup x

instance Parsable String [LTS] where
    from = fmap (fmap LTS) . readMaybe

instance Parsable String LTS where
    from = fmap LTS . readMaybe

interpret :: [(String, String)] -> ConfigLang LTS a -> ([LogMessage String], Either String a)
interpret = interpret' False [] . LTS

interpret' :: Bool -> [LogMessage String] -> LTS -> ConfigLang LTS a -> ([LogMessage String], Either String a)
interpret' _ io _ (Pure x) = (reverse io, Right x)
interpret' n' io kv' (Free f) = free f
  where
    free (Init _ g) = interpret' n' io kv' (g kv')
    free (Lookup kv k g) = maybe (lookupFail k) readM' $ dbLookup kv k
      where
        readM' x = maybe (parseFail n' k x) (interpret' n' io kv . g) $ from x
    free (WithDefault b f' g) =
      case interpret' True io kv' f' of
        (io', Left _) -> interpret' n' (reverse io') kv' (g b)
        (io', Right x) -> interpret' n' (reverse io') kv' (g x)
    free (Validate k b g) =
      case (k b, n') of
        (True, _) -> interpret' n' io kv' $ g b
        (_, False) ->
          interpret' n' io kv' $ logAndFail $ "Invalid value: " ++ show b
        (_, True) ->
          interpret' n' io kv' $ warnAndFail $ "Invalid value: " ++ show b
    free (ConfigLog s a) = interpret' n' (s : io) kv' a
    free (FailConfig s _) = (reverse io, Left s)
    lookupFail k = interpret' n' io kv' (logAndFail $ cannotLookup k)
    parseFail True k s = interpret' n' io kv' (warnAndFail $ cannotParse k s)
    parseFail False k s = interpret' n' io kv' (logAndFail $ cannotParse k s)
    warnAndFail m = configLog (messageWarn m) >> failConfig m
    logAndFail m = configLog (messageError m) >> failConfig m
    cannotLookup k = "Cannot lookup " ++ show k
    cannotParse k s =
      "Cannot parse the value of " ++ show k ++ " from string: " ++ show s
