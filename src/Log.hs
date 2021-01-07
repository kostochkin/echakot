{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Log (
      LogLevel
    , LogMessage
    , logError
    , logInfo
    , logDebug
    , Stringable
    ) where

data LogLevel = None | Debug | Info | Error deriving (Eq, Ord, Show)

data LogMessage a = LogMessage {
        level :: LogLevel,
        body :: a
    }

class Stringable a where
    toString :: a -> String


instance Stringable String where
    toString = id

instance Stringable a => Show (LogMessage a) where
    show m = "[" ++ show (level m) ++ "] " ++ toString (body m)

instance Functor LogMessage where
    fmap f a = a { body = f $ body a }

logError :: a -> LogMessage a
logError = LogMessage Error

logInfo :: a -> LogMessage a
logInfo = LogMessage Info

logDebug :: a -> LogMessage a
logDebug = LogMessage Debug

