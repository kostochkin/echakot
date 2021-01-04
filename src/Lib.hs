{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import qualified System.IO as SIO
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Free


-- class (Monad m) => Messenger m where
--     readMessages :: m String
--     sendMessage :: String -> m String
-- 
-- echo :: (Messenger m) => String -> m String
-- echo = sendMessage
-- 
-- bot :: Messenger m => m String
-- bot = readMessages >>= echo 

-- newtype ListState a = ListState (StateT ([String], Integer) [] a) deriving (Functor, Applicative, Monad)
-- 
-- 
-- instance Messenger ListState where
--     readMessages = 
--         ListState $ do
--             (ms, n) <- get
--             put ([], n)
--             return $ head ms
--     sendMessage = undefined
-- 
-- 
-- 
--data MessageStream a = MessageStream { message :: a,
--                                       stream  :: MessageStream a}
--
--instance Show a => Show (MessageStream a) where
--    show x = "MessageStream { message = " ++ show (message x) ++", stream = ... }"
--
--instance Functor MessageStream where
--    fmap f m = MessageStream { message = f $ message m, stream = fmap f $ stream m }
--
--instance Applicative MessageStream where
--    pure x = MessageStream { message = x, stream = pure x}
--    f <*> x = MessageStream { message = message f (message x), stream = stream f <*> stream x }
--
--instance Monad MessageStream where
--    return = pure
--    x >>= k = msInterleave (k $ message x) (stream x >>= k)
--
--msInterleave :: MessageStream a -> MessageStream a -> MessageStream a
--msInterleave s1 s2 = MessageStream { message = message s1,
--                                     stream = interleaved } where
--    interleaved = MessageStream { message = message s2,
--                                  stream = msInterleave (stream s1) (stream s2) }
--
--
--msFromList [] = error "Empty stream"
--msFromList l = helper l where
--    helper [x] = MessageStream { message = x, stream = helper l }
--    helper (x:xs) = MessageStream {message = x, stream = helper xs}
--
--msToList :: MessageStream a -> [a]
--msToList m = message m : msToList (stream m)
--
--msTake :: Int -> MessageStream a -> [a]
--msTake n = take n . msToList
--
--data Transport m a b = Transport { getMessage :: m a, sendMessage :: b -> m () }
--
--data Messenger m a b = Messenger {
--        transport :: Transport m a b,
--        makeEcho :: a -> m b
--    }
--
--stdioTransport :: Transport IO String String
--stdioTransport = Transport { getMessage = getLine, sendMessage = putStrLn }
--
--ioMessenger :: Messenger IO String String
--ioMessenger = Messenger { transport = stdioTransport, makeEcho = return }
--
--echoBot :: (Monad m) => Messenger m a b -> m ()
--echoBot m = do
--    a <- getMessage $ transport m
--    b <- makeEcho m a
--    sendMessage (transport m) b

data Action = Echo | Help | TellRepeat | ModifyRepeat Int deriving Show

data BotApiF b a = GetMessage (b -> a)
                 | SendMessage b a 
                 | SelectAction b (Action -> a)

instance (Show b) => Show (BotApiF b a) where
    show (GetMessage _) = "Getting message ..."
    show (SendMessage x a) = "Sending message " ++ show x
    show (SelectAction x a) = "Selecting action for message " ++ show x
    

type BotApi b = Free (BotApiF b)

instance Functor (BotApiF b) where
    fmap f (GetMessage k) = GetMessage $ f . k
    fmap f (SendMessage b x) = SendMessage b $ f x
    fmap f (SelectAction b k) = SelectAction b $ f . k


sendMessage :: b -> BotApi b ()
sendMessage b = liftF $ SendMessage b ()

getMessage :: BotApi b b
getMessage = liftF $ GetMessage id 

selectAction :: b -> BotApi b Action
selectAction b = liftF $ SelectAction b id

strToAction :: String -> Action
strToAction s = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ try (reads si)
                    _               -> Echo
    where
        try [(x, "")] = Just x
        try _ = Nothing



listBotApi :: Show r => BotApi String r -> [String] -> [String]
listBotApi (Pure x) xs = []
listBotApi (Free a@(SendMessage str t)) xs = show a : listBotApi t xs
listBotApi (Free (GetMessage f)) [] = []
listBotApi (Free a@(GetMessage f)) (x:xs) = (show a ++ x) : listBotApi (f x) xs
listBotApi (Free a@(SelectAction b f)) xs = show a : listBotApi (f (strToAction b)) xs

stdioBotApi :: BotApi String () -> IO ()
stdioBotApi (Pure x) = return ()
stdioBotApi (Free a@(SendMessage str t)) = putStrLn str >> stdioBotApi t
stdioBotApi (Free a@(GetMessage f)) = getLine >>= stdioBotApi . f
stdioBotApi (Free a@(SelectAction str f)) = return (strToAction str) >>= stdioBotApi . f 



bot1 :: BotApi String ()
bot1 = do
    a <- getMessage
    c <- selectAction a
    case c of
        Help           -> sendMessage "This is help"
        TellRepeat     -> sendMessage "Keyboard"
        ModifyRepeat i -> sendMessage $ "Modified to " ++ show i
        Echo           -> sendMessage a



bot2 :: BotApi String ()
bot2 = forever bot1

someFunc :: IO ()
someFunc = do
    print $ listBotApi bot2 ["/help", "12234", "/repeat", "89890", "/repeat 5", "18989"]
    stdioBotApi bot2
    return ()

