{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Config where

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

interpret :: (IsString (Key v), Monad m, Semiredis v) => v -> Logger m String () -> ConfigLang v a -> m (Maybe a)
interpret _ l (Pure x) = inlineLogInfo "Config complete" l *>> return (Just x)
interpret c' l (Free f) = free f 
  where
    free (Init s g) = inlineLogInfo ("Run config: " ++ s) l *>> interpret c' l (g c')
    free (Lookup c x g) = maybe failLookup (maybe failParse (interpret c' l . g) . from) $ dbLookup c (fromString x)
    free (WithDefault b k g) = do
        e <- inlineLogDebug "With default" l *>> interpret c' l k
        case e of
            Nothing -> inlineLogWarn "Used default" l *>> interpret c' l (g b)
            Just x -> inlineLogInfo "Got value" l *>> interpret c' l (g x)
    free (Validate f' a g) = if f' a then interpret c' l (g a) else return Nothing
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = inlineLogError s l *>> return Nothing
    failLookup = inlineLogError "Lookup failed" l *>> return Nothing
    failParse = inlineLogError "Fail parse" l *>> return Nothing

