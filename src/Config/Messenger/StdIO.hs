module Config.Messenger.StdIO (
      newMessenger
    , newMessengerState
    ) where

import Config.App ( 
      App
    , newApp
    )
import Control.Concurrent.MVar (
      MVar
    , newMVar
    )
import Text.Read ( readMaybe )

newMessenger :: String -> String -> [Int] -> App Int ()
newMessenger s1 s2 xs = newApp () s1 s2 [(x, show x) | x <- xs]

newMessengerState :: String -> IO (MVar Int)
newMessengerState = newMVar . maybe 1 id . readMaybe

