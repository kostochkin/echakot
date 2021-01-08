{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Log.Message (
      LogLevel
    , LogMessage
    , logError
    , logInfo
    , logDebug
    , Stringable
    , checkLevelF
    , checkLevel
    , defaultLogLevel
    ) where

data LogLevel = None | Error | Info | Debug deriving (Eq, Ord, Show, Read)

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

defaultLogLevel :: LogLevel
defaultLogLevel = Info

logError :: a -> LogMessage a
logError = LogMessage Error

logInfo :: a -> LogMessage a
logInfo = LogMessage Info

logDebug :: a -> LogMessage a
logDebug = LogMessage Debug

checkLevelF :: LogLevel -> (a -> LogMessage a) -> Maybe (a -> LogMessage a)
checkLevelF l f = maybe Nothing (const $ Just f) $ checkLevel l (error "This error should never happen. Contact maintainer of the compiler")

checkLevel :: LogLevel -> LogMessage a -> Maybe (LogMessage a)
checkLevel cl m@(LogMessage{ level = ml })
    | cl < ml   = Nothing
    | otherwise = Just $ m

