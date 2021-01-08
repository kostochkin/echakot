module Log.Logger (
      Logger
    , loggerFromString
    , checkLevel
    , maybeLog
    , maybeMessage
    , maybeLogValue
    ) where

import qualified Log.Message as M (LogLevel, defaultLogLevel, checkLevel, LogMessage)

data Logger = Logger {
    currentLogLevel :: M.LogLevel
} deriving Show

maybeLog :: Logger -> m b -> (M.LogMessage a -> m b) -> M.LogMessage a -> m b
maybeLog l els thn m = maybe els thn (checkLevel l m)

maybeLogValue :: (Monad m) => Logger -> m a -> M.LogMessage (a -> b) -> (M.LogMessage b -> m ()) -> m a
maybeLogValue l ma lm logValue = maybe ma k' (checkLevel l lm) where
    k' _ = do
        x <- ma
        logValue $ fmap ($ x) lm
        return x 

maybeMessage :: Logger -> (b -> Maybe (M.LogMessage a)) -> b -> Maybe (M.LogMessage a)
maybeMessage l f b = f b >>= checkLevel l

loggerFromString :: String -> Logger
loggerFromString s = case reads s of
    [(x, "")] -> Logger x
    _         -> Logger M.defaultLogLevel

checkLevel :: Logger -> M.LogMessage a -> Maybe (M.LogMessage a)
checkLevel l m = M.checkLevel (currentLogLevel l) m

