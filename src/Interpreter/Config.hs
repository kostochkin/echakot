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

newtype InterpretException = InterpretException String deriving (Typeable)

instance Exception InterpretException

instance Show InterpretException where
    show (InterpretException s) = s
    
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
           dropException (inlineLogWarn "Using default" l *>> return b)
        interpret c' l (g x)
    free (Validate f' a g) = if f' a then interpret c' l (g a) else throwIE "Invalid value"
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = inlineLogError s l *>> throwIE "Fail"
    failLookup = inlineLogError "Lookup failed" l *>> throwIE "Lookup failed"
    failParse = inlineLogError "Parsing failed" l *>> throwIE "Parsing failed"

dropException :: m a -> InterpretException -> m a
dropException = const 

