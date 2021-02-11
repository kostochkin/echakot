{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.ConfigJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Text

import Language.Config
  ( ConfigLang
  , Parsable
  , from
  , Semiredis
  , Key
  , Val
  , dbLookup
  , LoggerType(..)
  )
import Data.Scientific (toBoundedInteger)
import Data.Text.Conversions (convertText)
import qualified Data.ByteString.Lazy.Char8 as CH
import Log.Logger
import qualified Text.Read as TR
import qualified Interpreter.Config as IC (interpret)

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

instance Parsable JsonVal LoggerType where
    from (JsonVal (String s)) = TR.readMaybe $ unpack s
    from _ = Nothing


interpret :: Value -> Logger IO String () -> ConfigLang JsonVal a -> IO a
interpret v l f = IC.interpret (JsonVal v) l f

