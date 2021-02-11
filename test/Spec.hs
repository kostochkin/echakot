import Control.Monad (forever)
import qualified Interpreter.Test.BotApi as BAT
import qualified Interpreter.Test.Config as BAC
import qualified Language.Config as LCF
import qualified Log.Logger as LL
import qualified Program.Config as PCF
import qualified Program.FsdEchoBot as FE
import Control.Error.Safe

main :: IO ()
main = do
  putStrLn ""
  testEqual botApiTest botApiInput botApiOutput "Test Bot Logic"
  testJust readLoggerTest configApp "Test read logger from config"
  testNothing
    readLoggerTest
    configAppLoggerFail
    "Test read logger from config must fail"

testEqual :: Eq b => (a -> b) -> a -> b -> String -> IO ()
testEqual f i o s =
  putStrLn $
  if f i == o
    then s ++ ": OK"
    else s ++ ": FAIL!"

testJust :: (a -> Maybe c) -> a -> String -> IO ()
testJust f a s = putStrLn $ maybe (s ++ ": FAIL!") (const $ s ++ ": OK") (f a)

testNothing :: (a -> Maybe c) -> a -> String -> IO ()
testNothing f a s = putStrLn $ maybe (s ++ ": OK!") (const $ s ++ ": FAIL") (f a)

-- Bot api test data
botApiTest = uncurry $ BAT.interpret (forever FE.botStep)

botApiInput =
  ( ( 1
    , [ ["/help"]
      , ["111", "/repeat"]
      , ["222", "/repeat 2", "/repeat"]
      , ["333"]
      , ["/repeat 3", "444"]
      , ["/repeat -1", "555", "/repeat 10"]
      , ["666"]
      ])
  , [1 .. 5])

botApiOutput =
  [ "help"
  , "111"
  , "1"
  , "keyboard"
  , "222"
  , "2"
  , "keyboard"
  , "333"
  , "333"
  , "444"
  , "444"
  , "444"
  , "/repeat -1"
  , "/repeat -1"
  , "/repeat -1"
  , "555"
  , "555"
  , "555"
  , "/repeat 10"
  , "/repeat 10"
  , "/repeat 10"
  , "666"
  , "666"
  , "666"
  ]

-- Config test data
loggers =
  [ [("type", "Stdio"), ("logLevel", "None")]
  , [("type", "File"), ("logLevel", "Debug"), ("filename", "echobot-test.log")]
  ]

loggerFail =
  [ [("type", "Stdio"), ("logLevel", "None")]
  , [("type", "file"), ("logLevel", "Debug"), ("filename", "echobot-test-fail.log")]
  ]

messengerC = [("type", "Stdio"), ("showingKeyboardMessage", "keyboard")]

botGeneral =
  [("repeats", "1"), ("repeatsMessage", "repeats"), ("helpMessage", "help")]

configApp =
  [ ("loggers", show loggers)
  , ("messenger", show messengerC)
  , ("botGeneral", show botGeneral)
  ]

configAppLoggerFail =
  [ ("loggers", show loggerFail)
  , ("messenger", show messengerC)
  , ("botGeneral", show botGeneral)
  ]

readLoggerTest :: [(String, String)] -> Maybe (IO (LL.Logger IO String ()))
readLoggerTest c = rightMay $ BAC.interpret c PCF.initLoggingConfig

