module Config.Messenger.StdIO (
      newMessenger
    , newMessengerState
    , StdioMessenger
    , state
    ) where

import Config.App ( 
      App
    , newApp
    )
import Control.Concurrent.MVar (
      MVar
    , newMVar
    )
import Interpreter.Actions ( repeatsMaybe )

newtype StdioMessenger = StdioMessenger { state :: MVar Int }

newMessenger :: String -> String -> String -> [Int] -> Int -> IO (App Int StdioMessenger)
newMessenger kbdm hm rm kbd i = do
    s <- newMessengerState kbd i
    return $ newApp (StdioMessenger s) hm rm kbdm [(x, show x) | x <- kbd]

    
newMessengerState :: [Int] -> Int -> IO (MVar Int)
newMessengerState []  = newMessengerState [1]
newMessengerState kbd = newMVar . maybe (head kbd) id . repeatsMaybe kbd

