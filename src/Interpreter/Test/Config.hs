{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter.Test.Config
  ( interpret
  ) where

import Language.Config
  ( ConfigLang
  , Parsable
  , from
  , Semiredis (Key, Val)
  , dbLookup
  , LoggerType
  )
import Log.Logger (loggerFromString, Logger)
import Text.Read (readMaybe)
import Data.String
import qualified Interpreter.Config as IC (interpret, SomeException)

newtype LTS = LTS [(String, String)]

newtype LTSString = LTSString { unpack :: String }

instance Show LTS where
    show (LTS x) = show x

instance Show LTSString where
    show = unpack

instance IsString LTSString where
    fromString = LTSString

instance Semiredis LTS where
    type Key LTS = LTSString
    type Val LTS = LTSString
    dbLookup (LTS x) = fmap LTSString . flip lookup x . unpack

instance Parsable LTSString [LTS] where
    from = fmap (fmap LTS) . readMaybe . unpack

instance Parsable LTSString LTS where
    from = fmap LTS . readMaybe . unpack

instance Parsable LTSString LoggerType where
    from = readMaybe . unpack

instance Parsable LTSString String where
    from = Just . unpack


dummyLogger :: Logger (Either IC.SomeException) String ()
dummyLogger = loggerFromString (const $ return ()) "Debug"

interpret :: [(String, String)] -> ConfigLang LTS a -> Either IC.SomeException a
interpret kv p = IC.interpret (LTS kv) dummyLogger p


