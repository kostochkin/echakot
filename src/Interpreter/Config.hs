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
import Log.Logger ( Logger, (*>>), (|>), inlineLogError, inlineLogWarn, inlineLogDebug, inlineLogInfo )
import Data.String ( IsString, fromString )
import Control.Monad.Catch ( Exception, MonadCatch, MonadThrow, catch, throwM, SomeException )
import Type.Reflection ( Typeable )

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
    free (Init s g) = inlineLogInfo ("Interpreter: Run config: " ++ s) l *>> interpret c' l (g c')
    free (Lookup c x g) = maybe failLookup (maybe failParse (interpret c' l . g) . from) $ dbLookup c (fromString x)
    free (WithDefault b k g) = do
        x <- (inlineLogDebug "Interpreter: With default" l *>> interpret c' l k) `catch`
           dropException (inlineLogWarn "Interpreter: Using default" l *>> return b)
        interpret c' l (g x)
    free (Validate f' a g) = if f' a then interpret c' l (g a) else throwIE "Intrepreter: Invalid value"
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = inlineLogError s l *>> throwIE "Fail"
    failLookup = inlineLogError "Interpreter: Lookup failed" l *>> throwIE "Lookup failed"
    failParse = inlineLogError "Interpreter: Parsing failed" l *>> throwIE "Parsing failed"

dropException :: m a -> InterpretException -> m a
dropException = const 

