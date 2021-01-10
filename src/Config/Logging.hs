module Config.Logging (
      newLoggingConfig
    , LoggingConfig
    , logger
    , logLevelString
    ) where

import Log.Logger ( Logger )

data LoggingConfig m a b = LoggingConfig {
      logger' :: Logger m a b
    , logLevel' :: String
    }

instance Show (LoggingConfig m a b) where
    show = logLevel'

logger :: LoggingConfig m a b -> Logger m a b
logger = logger'

logLevelString :: LoggingConfig m a b -> String
logLevelString = logLevel'

newLoggingConfig :: Logger m a b -> String -> LoggingConfig m a b 
newLoggingConfig = LoggingConfig

