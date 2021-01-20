{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}

module Language.Config where

import Control.Monad.Free ( Free( Pure ), liftF )
import Log.Message ( LogMessage )


data ConfigLangF k c a = Init String (c -> a)
                       | LookupString c k (String -> a)
                       | forall b. (Read b, Show b) => Lookup c k (b -> a)
                       | forall b. (Read b, Show b) => LookupDefault c k b (b -> a)
                       | forall b. (Read b, Show b) => WithDefault b (ConfigLang k c b) (b -> a)
                       | forall b. (Read b, Show b) => Validate (b -> Bool) b (b -> a)
                       | forall b. FailConfig String (b -> a)
                       | ConfigLog (LogMessage String) a

instance Functor (ConfigLangF k c) where
    fmap f (Init s g) = Init s (fmap f g)
    fmap f (LookupString c k g) = LookupString c k (fmap f g)
    fmap f (Lookup c k g) = Lookup c k (fmap f g)
    fmap f (LookupDefault c k b g) = LookupDefault c k b (fmap f g)
    fmap f (WithDefault b f' g) = WithDefault b f' (fmap f g)
    fmap f (Validate k b g) = Validate k b (fmap f g)
    fmap f (ConfigLog m a) = ConfigLog m (f a)
    fmap f (FailConfig m a) = FailConfig m (fmap f a)


type ConfigLang k c = Free (ConfigLangF k c)

init :: String -> ConfigLang k c c
init s = liftF $ Init s id

lookup :: (Show b, Read b) => c -> k -> ConfigLang k c b
lookup c k = liftF $ Lookup c k id

lookupString :: c -> k -> ConfigLang k c String
lookupString c k = liftF $ LookupString c k id

lookupDefault :: (Show b, Read b) => c -> k -> b -> ConfigLang k c b
lookupDefault c k a = liftF $ LookupDefault c k a id

withDefault :: (Read b, Show b) => b -> ConfigLang k c b -> ConfigLang k c b
withDefault b f = liftF $ WithDefault b f id

validate :: (Show b, Read b) => (b -> Bool) -> b -> ConfigLang k c b
validate f b = liftF $ Validate f b id

returnConfig :: a -> ConfigLang c k a
returnConfig = Pure

failConfig :: String -> ConfigLang c k a
failConfig s = liftF $ FailConfig s id

configLog :: LogMessage String -> ConfigLang c k ()
configLog m = liftF $ ConfigLog (fmap ("Config: " ++) m) ()

