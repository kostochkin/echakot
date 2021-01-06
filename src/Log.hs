module Log where

data LogLevel = None | Debug | Info | Error deriving (Eq, Ord, Show)

class Loggable a where
    tryLog   :: a -> Maybe String
    logLevel :: LogLevel -> a -> Maybe String
    logDebug :: a -> Maybe String
    logError :: a -> Maybe String
    logLevel l a = fmap (("[" ++ show l ++ "] ") ++) $ tryLog a
    logDebug = logLevel Debug
    logError = logLevel Error

