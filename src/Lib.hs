module Lib
    ( someFunc
    ) where

import  Messenger.Api as M
    (Api (..),
     Message(..),
     keyboard
    )

import Messenger.Api ((#))

import Messenger.StdIO
    ( runStdIOMessenger
    )
import Control.Monad
    ( forever
    )

kbd = keyboard # 1 # 2 # 3 # 4 # 5

simpleEchoBot :: (M.Api m) => m ()
simpleEchoBot = do
    x <- receiveMessage kbd
    replyMessage x

replyMessage :: (M.Api m) => M.Message -> m ()
replyMessage x = case x of
    Message _ -> sendMessage x
    Key k -> sendMessage $ M.Message ("Repeats received " ++ show k ++ ". Changing state is not implemented yet.")
    Help -> replyHelp
    Repeat -> replyRepeat

replyHelp :: (M.Api m) => m ()
replyHelp = mapM_ (sendMessage . M.Message)
    ["Hello! I'm an echo bot.",
     "My name is Echokot.",
     "Enter \\help to get this message again",
     "Enter \\repeat to get my current state",
     "Enter any text and I'll repeat it",
     "What is your name?"]

replyRepeat :: (M.Api m) => m ()
replyRepeat = do
    let current = 1
    sendMessage $ M.Message $ "Current repeats " ++ show current ++ ". Available options are shown below:"
    showKeyboard kbd

someFunc = forever $ runStdIOMessenger simpleEchoBot

