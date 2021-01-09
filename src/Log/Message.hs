{-# LANGUAGE DeriveFunctor #-}

module Log.Message (
      LogLevel
    , LogMessage
    , messageError
    , messageInfo
    , messageDebug
    , bounceMessage
    , defaultLogLevel
    ) where

data LogLevel = None | Error | Info | Debug deriving (Eq, Ord, Show, Read)

data LogMessage a = LogMessage {
        level :: LogLevel,
        body :: a
    } deriving (Functor)

instance Show a => Show (LogMessage a) where
    show m = "[" ++ show (level m) ++ "] " ++ show (body m)


defaultLogLevel :: LogLevel
defaultLogLevel = Info

messageError :: a -> LogMessage a
messageError = LogMessage Error

messageInfo :: a -> LogMessage a
messageInfo = LogMessage Info

messageDebug :: a -> LogMessage a
messageDebug = LogMessage Debug

bounceMessage :: LogLevel -> LogMessage a -> Maybe (LogMessage a)
bounceMessage cl m = if cl < level m then Nothing else Just m

