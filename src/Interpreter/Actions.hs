module Interpreter.Actions
  ( strToAction
  , repeatsMaybe
  , TimeTagged
  , toTimeTagged
  , timeTaggedLogger
  , tagLogger
  ) where

import Control.Monad (guard)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Language.Bot (Action(Echo, Help, ModifyRepeat, TellRepeat))
import Text.Read (readMaybe)
import Log.Logger

data TimeTagged =
  TimeTagged
    { time :: UTCTime
    , body :: String
    }

instance Show TimeTagged where
  show x = "[" ++ show (time x) ++ "] " ++ body x

strToAction :: (Read i, Eq i) => String -> [i] -> Action i
strToAction s k =
  case words s of
    ["/help"] -> Help
    ["/repeat"] -> TellRepeat
    ["/repeat", si] -> maybe Echo ModifyRepeat $ readRepeatsMaybe k si
    _ -> Echo

repeatsMaybe :: (Eq i) => [i] -> i -> Maybe i
repeatsMaybe k i = guard (i `elem` k) >> return i

readRepeatsMaybe :: (Read i, Eq i) => [i] -> String -> Maybe i
readRepeatsMaybe k s = readMaybe s >>= repeatsMaybe k

toTimeTagged :: IO (String -> TimeTagged)
toTimeTagged = do
  t <- getCurrentTime
  return $ \s -> TimeTagged {time = t, body = s}

timeTaggedLogger :: String -> Logger IO String ()
timeTaggedLogger l = loggerFromString print' l where
    print' m = do
        t <- toTimeTagged
        print $ fmap t m

tagLogger :: Logger IO TimeTagged () -> Logger IO String ()
tagLogger l = loggerFromString print' (currentLogLevel l) where
    print' m = do
        t <- toTimeTagged
        fmap t m |> l *>> return ()
