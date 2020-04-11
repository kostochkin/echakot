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
import Bot.StateClass as B

kbd = keyboard # 1 # 2 # 3 # 4 # 5

simpleEchoBot :: (B.BotStateClass m, M.Api m) => m ()
simpleEchoBot = receiveMessage kbd >>= processMessage

processMessage :: (B.BotStateClass m, M.Api m) => M.Message -> m ()
processMessage x = case x of
    Message _ -> echoMessage x
    Key k -> setBotRepeats k
    Help -> replyHelp
    Repeat -> replyRepeat

setBotRepeats :: (B.BotStateClass m, M.Api m) => Int -> m ()
setBotRepeats k = setRepeats k >> sendMessage (Message "Ok")

echoMessage :: (B.BotStateClass m, M.Api m) => Message -> m ()
echoMessage x = do
    n <- getRepeats
    mapM_ sendMessage $ replicate n x

replyHelp :: (B.BotStateClass m, M.Api m) => m ()
replyHelp = mapM_ (sendMessage . M.Message)
    ["Hello! I'm an echo bot.",
     "My name is Echakot.",
     "Enter \\help to get this message again",
     "Enter \\repeat to get my current state",
     "Enter any text and I'll repeat it",
     "What is your name?"]

replyRepeat :: (B.BotStateClass m, M.Api m) => m ()
replyRepeat = do
    current <- getRepeats
    sendMessage $ M.Message $ "Current repeats " 
                              ++ show current
                              ++ ". Modify it by entering one of the following commands:"
    showKeyboard kbd

someFunc = (runStdIOMessenger $ runBotStateT (forever simpleEchoBot) BotSt {repeats = 1}) >> return ()

