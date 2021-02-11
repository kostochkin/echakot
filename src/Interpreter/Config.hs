{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Config (interpret, SomeException, throwIE) where

import Control.Monad.Free (Free(Free, Pure))
import Language.Config
  ( ConfigLang
  , ConfigLangF(ConfigLog, FailConfig, Init, Lookup, Validate, WithDefault)
  , from
  , Semiredis
  , Key
  , dbLookup
  )
import Log.Logger
import Data.String
import Control.Monad.Catch
import Type.Reflection

newtype InterpretException = InterpretException String deriving (Typeable, Show)

instance Exception InterpretException
    
throwIE :: (MonadThrow m) => String -> m a
throwIE = throwM . InterpretException


interpret :: (IsString (Key v), MonadCatch m, Semiredis v) => v -> Logger m String () -> ConfigLang v a -> m a
interpret _ l (Pure x) = inlineLogInfo "Config complete" l *>> return x
interpret c' l (Free f) = free f 
  where
    free (Init s g) = inlineLogInfo ("Run config: " ++ s) l *>> interpret c' l (g c')
    free (Lookup c x g) = maybe failLookup (maybe failParse (interpret c' l . g) . from) $ dbLookup c (fromString x)
    free (WithDefault b k g) = do
        x <- (inlineLogDebug "With default" l *>> interpret c' l k) `catch`
           \e -> inlineLogWarn (show (e :: InterpretException)) l *>> return b
        interpret c' l (g x)
    free (Validate f' a g) = if f' a then interpret c' l (g a) else throwIE "Invalid"
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = inlineLogError s l *>> throwIE "Fail"
    failLookup = inlineLogError "Lookup failed" l *>> throwIE "Fail"
    failParse = inlineLogError "Fail parse" l *>> throwIE "Fail"

