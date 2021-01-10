module Config.App (
      App
    , newApp
    , helpReply
    , repeatReply
    , keyboard
    , messenger
    ) where


data App i a = App {
      messenger'   :: a
    , helpReply' :: String
    , repeatReply' :: String
    , keyboard'    :: [(i, String)]
    }   

newApp :: a -> String -> String -> [(i, String)] -> App i a
newApp = App

helpReply :: App i a -> String
helpReply = helpReply'

repeatReply :: App i a -> String
repeatReply = repeatReply'

keyboard :: App i a -> [(i, String)]
keyboard = keyboard'

messenger :: App i a -> a
messenger = messenger'


