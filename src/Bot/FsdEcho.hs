module Bot.FsdEcho (
    fsdEchoBotStep
    ) where

import qualified Bot.StateClass as B

import qualified Messenger.Api as M
    (Api (..),
     Message(..),
     keyboard
    )

import Messenger.Api ((#))

kbd = M.keyboard # 1 # 2 # 3 # 4 # 5

fsdEchoBotStep :: (B.BotStateClass m, M.Api m) => m ()
fsdEchoBotStep = M.receiveMessage kbd >>= processMessage

processMessage :: (B.BotStateClass m, M.Api m) => M.Message -> m ()
processMessage x = case x of
    M.Message _ -> echoMessage x
    M.Key k -> setBotRepeats k
    M.Help -> replyHelp
    M.Repeat -> replyRepeat

setBotRepeats :: (B.BotStateClass m, M.Api m) => Int -> m ()
--setBotRepeats k = B.setRepeats k >> M.sendMessage (M.Message "Ok")
setBotRepeats k = B.setRepeats k

echoMessage :: (B.BotStateClass m, M.Api m) => M.Message -> m ()
echoMessage x = do
    n <- B.getRepeats
    mapM_ M.sendMessage $ replicate n x

replyHelp :: (B.BotStateClass m, M.Api m) => m ()
replyHelp = mapM_ (M.sendMessage . M.Message)
    ["Hello! I'm an echo bot.",
     "My name is Echakot.",
     "Enter \\help to get this message again",
     "Enter \\repeat to get my current state",
     "Enter any text and I'll repeat it",
     "What is your name?"]

replyRepeat :: (B.BotStateClass m, M.Api m) => m ()
replyRepeat = do
    current <- B.getRepeats
    M.sendMessage $ M.Message $ "Current repeats " 
                                ++ show current
                                ++ ". Modify it by entering one of the following commands:"
    M.showKeyboard kbd

