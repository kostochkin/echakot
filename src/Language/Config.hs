{-# LANGUAGE DeriveFunctor, ExistentialQuantification #-}

module Language.Config where

import Control.Monad.Free ( Free ( Pure, Free ), liftF )
import Log.Message ( LogMessage )
import Text.Read


data ConfigLangF k c a = Init String (c -> a)
                       | LookupString c k (String -> a)
                       | forall b. (Read b) => Lookup c k (b -> a)
                       | forall b. (Read b) => LookupDefault c k b (b -> a)
                       | forall b. FailConfig String (b -> a)
                       | ConfigLog (LogMessage String) a



instance Functor (ConfigLangF k c) where
    fmap f (Init s g) = Init s (fmap f g)
    fmap f (LookupString c k g) = LookupString c k (fmap f g)
    fmap f (Lookup c k g) = Lookup c k (fmap f g)
    fmap f (LookupDefault c k b g) = LookupDefault c k b (fmap f g)
    fmap f (ConfigLog m a) = ConfigLog m (f a)
    fmap f (FailConfig m a) = FailConfig m (fmap f a)


type ConfigLang k c = Free (ConfigLangF k c)

init :: String -> ConfigLang k c c
init s = liftF $ Init s id

lookup :: (Read b) => c -> k -> ConfigLang k c b
lookup c k = liftF $ Lookup c k id

lookupString :: c -> k -> ConfigLang k c String
lookupString c k = liftF $ LookupString c k id

lookupDefault :: (Read b) => c -> k -> b -> ConfigLang k c b
lookupDefault c k a = liftF $ LookupDefault c k a id

returnConfig :: a -> ConfigLang c k a
returnConfig = Pure

failConfig :: String -> ConfigLang c k a
failConfig s = liftF $ FailConfig s id

configLog :: LogMessage String -> ConfigLang c k ()
configLog m = liftF $ ConfigLog (fmap ("Config: " ++) m) ()


dummyInterpret :: [(String, String)] -> ConfigLang String [(String, String)] a -> Either String a
dummyInterpret _ (Pure x) = Right x
dummyInterpret kv' (Free f) = free f where
    free (Init _ g) = dummyInterpret kv' (g kv')
    free (LookupString kv k g) = maybe (lookupFail k) (dummyInterpret kv . g) $ Prelude.lookup k kv
    free (Lookup kv k g) = maybe (lookupFail k) readM' $ Prelude.lookup k kv where
        readM' x = maybe (parseFail k x) (dummyInterpret kv . g) $ readMaybe x
    free (LookupDefault kv k b g) = dummyInterpret kv $ g $ maybe b id (Prelude.lookup k kv >>= readMaybe)
    free (ConfigLog _ a) = dummyInterpret kv' a
    free (FailConfig s _) = Left s 
    lookupFail k = Left $ "Cannot lookup " ++ k
    parseFail k s = Left $ "Cannot parse the value of " ++ k ++ " from string: " ++ s

