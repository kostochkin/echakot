{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.ConfigJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.String

import Control.Monad.Free (Free(Free, Pure))
import Language.Config
  ( ConfigLang
  , ConfigLangF(ConfigLog, FailConfig, Init, Lookup, Validate, WithDefault)
  , configLog
  , Parseable
  , from
  , Semiredis
  , Key
  , Val
  , dbLookup
  )
import Log.Message ( messageError )
import Data.Scientific (toBoundedInteger)
import Data.Text.Conversions (convertText)
import qualified Data.ByteString.Lazy.Char8 as CH
import Log.Logger

newtype JsonVal = JsonVal Value

instance Show JsonVal where
    show (JsonVal x) = CH.unpack $ encode x

instance Semiredis JsonVal where
    type Key JsonVal = Text
    type Val JsonVal = JsonVal
    dbLookup (JsonVal (Object o)) k = fmap JsonVal $ parseMaybe (.: k) o
    dbLookup _ _ = Nothing

instance Parseable JsonVal String where
    from (JsonVal (String s)) = Just $ convertText s
    from _ = Nothing

instance Parseable JsonVal Int where
    from (JsonVal (Number n)) = toBoundedInteger n
    from _ = Nothing

instance Parseable JsonVal [JsonVal] where
    from (JsonVal (Array os)) = Just $ Prelude.foldr ((:) . JsonVal) [] os
    from _ = Nothing

type R a = IO (Either String a)

interpret :: Value -> Logger IO String () -> ConfigLang JsonVal a -> R a
interpret _ _ (Pure x) = return $ Right x
interpret c' l (Free f) = free f 
  where
    free (Init _ g) = interpret c' l (g (JsonVal c'))
    free (Lookup c x g) = maybe (return $ Left "oops") (maybe (return $ Left "ops") (interpret c' l . g) . from) $ dbLookup c (fromString x)
    free (WithDefault b k g) = do
        e <- interpret c' l k
        interpret c' l $ g $ case e of
            Left _ -> b
            Right x -> x
    free (Validate f' a g) = if f' a then return (Left "not valid") else interpret c' l (g a)
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = interpret c' l (configLog $ messageError s) >> return (Left s)

