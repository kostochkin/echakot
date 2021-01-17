{-# LANGUAGE DeriveFunctor #-}

module Log.Logger (
      Logger
    , LoggerF
    , LoggerV
    , loggerFromString
    , runLogger
    , currentLogLevel
    , joinLoggers
    , inlineLogInfo
    , inlineLogError
    , inlineLogDebug
    , inlineLogInfoV
    , inlineLogErrorV
    , inlineLogDebugV
    , (|->)
    , (>>=*)
    , (*>>=)
    , (|>)
    , (*>>)
    ) where


import Text.Read (readMaybe)
import Log.Message (
      LogMessage
    , defaultLogLevel
    , LogLevel
    , bounceMessage
    , messageInfo
    , messageError
    , messageDebug
    )


data Logger m a b = Logger {
      handler :: LogMessage a -> m b
    , currentLogLevel' :: LogLevel
} deriving (Functor)

data LoggerF m a b c = LoggerF (LogMessage (a -> b)) (Logger m b c)

data LoggerV m a b = LoggerV (LogMessage a) (Logger m a b)

data LoggedA m a b c =  LoggedA (m a) (LoggerF m a b c)

currentLogLevel :: Logger m a b -> String
currentLogLevel = show . currentLogLevel'

joinLoggers :: (Monad m, Monoid b) => [Logger m a b] -> Logger m a b
joinLoggers xs = Logger { handler = \m -> handle m xs
                        , currentLogLevel' = maximum $ map currentLogLevel' xs} where
    handle _ [] = return mempty
    handle m (l:ls) = m |> l *>> handle m ls


infixl 9 |->
(|->) :: LogMessage (a -> b) -> Logger m b c ->  LoggerF m a b c
(|->) = LoggerF

infixl 9 |>
(|>) :: LogMessage a -> Logger m a b ->  LoggerV m a b
(|>) = LoggerV

infixl 2 >>=*
(>>=*) :: (Monad m) => m a -> LoggerF m a b c -> LoggedA m a b c
(>>=*) = LoggedA

infixl 2 *>>=
(*>>=) :: (Monad m) => LoggedA m a b d -> (a -> m c) -> m c
(LoggedA ma (LoggerF s l)) *>>= mac = logMITM ma mac l s

infixl 2 *>>
(*>>) :: (Monad m) => LoggerV m a b -> m c -> m c
(LoggerV s l) *>> mc = logBefore mc l s

inlineLogErrorV :: (a -> b) -> Logger m b c -> LoggerF m a b c
inlineLogErrorV f l = messageError f |-> l

inlineLogInfoV :: (a -> b) -> Logger m b c -> LoggerF m a b c
inlineLogInfoV f l = messageInfo f |-> l

inlineLogDebugV :: (a -> b) -> Logger m b c -> LoggerF m a b c
inlineLogDebugV f l = messageDebug f |-> l

inlineLogError :: a -> Logger m a b ->  LoggerV m a b
inlineLogError x l = messageError x |> l

inlineLogInfo ::  a -> Logger m a b ->  LoggerV m a b
inlineLogInfo x l = messageInfo x |> l

inlineLogDebug ::  a -> Logger m a b ->  LoggerV m a b
inlineLogDebug x l = messageDebug x |> l

loggerFromString :: (LogMessage a -> m b) -> String -> Logger m a b
loggerFromString k s = Logger k $ maybe defaultLogLevel id $ readMaybe s

runLogger :: Logger m a b -> LogMessage a -> m b
runLogger = handler

logBefore :: (Monad m) => m c -> Logger m a b -> LogMessage a -> m c
logBefore mc l lm = maybe mc k' (bounce l lm) where
    k' _ = do
        _ <- runLogger l lm
        mc

logMITM :: (Monad m) => m a -> (a -> m c) -> Logger m b d -> LogMessage (a -> b) -> m c
logMITM before after l lm = maybe (before >>= after) k' (bounce l lm) where
    k' _ = do
        x <- before
        _ <- runLogger l $ fmap ($ x) lm
        after x 

bounce :: Logger m a c -> LogMessage b -> Maybe (LogMessage b)
bounce l m = bounceMessage (currentLogLevel' l) m

