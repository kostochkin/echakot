{-# LANGUAGE ExistentialQuantification #-}

module Config.App
  ( App
  , newApp
  , helpReply
  , repeatReply
  , keyboardMessage
  , keyboard
  , messenger
  ) where

data App i a =
  App
    { messenger' :: a
    , helpReply' :: String
    , repeatReply' :: String
    , keyboardMessage' :: String
    , keyboard' :: [(i, String)]
    }

newApp :: a -> String -> String -> String -> [(i, String)] -> App i a
newApp = App

helpReply :: App i a -> String
helpReply = helpReply'

repeatReply :: App i a -> String
repeatReply = repeatReply'

keyboard :: App i a -> [(i, String)]
keyboard = keyboard'

messenger :: App i a -> a
messenger = messenger'

keyboardMessage :: App i a -> String
keyboardMessage = keyboardMessage'
