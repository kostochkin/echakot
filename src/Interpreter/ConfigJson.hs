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
  , Parsable
  , from
  , Semiredis
  , Key
  , Val
  , dbLookup
  , failConfig
  )
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

instance Parsable JsonVal String where
    from (JsonVal (String s)) = Just $ convertText s
    from _ = Nothing

instance Parsable JsonVal Int where
    from (JsonVal (Number n)) = toBoundedInteger n
    from _ = Nothing

instance Parsable JsonVal [JsonVal] where
    from (JsonVal (Array os)) = Just $ Prelude.foldr ((:) . JsonVal) [] os
    from _ = Nothing


interpret :: Value -> Logger IO String () -> ConfigLang JsonVal a -> IO (Either String a)
interpret _ _ (Pure x) = return $ Right x
interpret c' l (Free f) = free f 
  where
    free (Init _ g) = interpret c' l (g (JsonVal c'))
    free (Lookup c x g) = maybe (return $ Left $ "Cannot find: " ++ show x) (maybe (return $ Left $ "Can't parse: " ++ show x) (interpret c' l . g) . from) $ dbLookup c (fromString x)
    free (WithDefault b k g) = do
        e <- interpret c' l k
        case e of
            Left s -> inlineLogWarn s l *>> interpret c' l (g b)
            Right x -> interpret c' l (g x)
    free (Validate f' a g) = if f' a then interpret c' l (g a) else interpret c' l (failConfig "not valid")
    free (ConfigLog m g) = m |> l *>> interpret c' l g
    free (FailConfig s _) = return (Left s)

