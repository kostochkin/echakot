{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Config where

import Control.Monad.Free (Free(Pure), liftF)
import Log.Message (LogMessage)
import Text.Read (readMaybe)

class Parseable a b where
    from :: a -> Maybe b

class Semiredis a where
    type Key a
    type Val a
    dbLookup :: a -> Key a -> Maybe (Val a)

instance Parseable String String where
    from = Just

instance Parseable String Int where
    from = readMaybe

data ConfigLangF c a where
    Init :: String -> (c -> a) -> ConfigLangF c a
    Lookup :: (Semiredis c, Parseable (Val c) b, Show b, Show (Val c)) => c -> String -> (b -> a) -> ConfigLangF c a
    WithDefault :: (Semiredis c, Parseable (Val c) b, Show b) => b -> (ConfigLang c b) -> (b -> a) -> ConfigLangF c a
    Validate :: (Show b) => (b -> Bool) -> b -> (b -> a) -> ConfigLangF c a
    FailConfig :: String -> (b -> a) -> ConfigLangF c a
    ConfigLog :: (LogMessage String) -> a -> ConfigLangF c a
    
instance Functor (ConfigLangF c) where
  fmap f (Init s g) = Init s (fmap f g)
  fmap f (Lookup c k g) = Lookup c k (fmap f g)
  fmap f (WithDefault b f' g) = WithDefault b f' (fmap f g)
  fmap f (Validate k b g) = Validate k b (fmap f g)
  fmap f (ConfigLog m a) = ConfigLog m (f a)
  fmap f (FailConfig m a) = FailConfig m (fmap f a)

type ConfigLang c = Free (ConfigLangF c)

init :: String -> ConfigLang c c
init s = liftF $ Init s id

lookup :: (Semiredis c, Parseable (Val c) b, Show (Val c), Show b) => c -> String -> ConfigLang c b
lookup c k = liftF $ Lookup c k id

withDefault :: (Semiredis c, Parseable (Val c) b, Show b) => b -> ConfigLang c b -> ConfigLang c b
withDefault b f = liftF $ WithDefault b f id

validate :: (Show b) => (b -> Bool) -> b -> ConfigLang c b
validate f b = liftF $ Validate f b id

returnConfig :: a -> ConfigLang c a
returnConfig = Pure

failConfig :: String -> ConfigLang c a
failConfig s = liftF $ FailConfig s id

configLog :: LogMessage String -> ConfigLang c ()
configLog m = liftF $ ConfigLog (fmap ("Config: " ++) m) ()

