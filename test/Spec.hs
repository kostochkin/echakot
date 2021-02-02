import Control.Monad (forever)
import qualified Interpreter.Test.BotApi as BAT
import qualified Interpreter.Test.Config as BAC
import qualified Language.Config as LCF
import qualified Log.Logger as LL
import qualified Program.Config as PCF
import qualified Program.FsdEchoBot as FE

main :: IO ()
main = do
  putStrLn ""
  testEqual botApiTest botApiInput botApiOutput "Test Bot Logic"
  testRight readLoggerTest configApp "Test read logger from config"
  testLeft
    readLoggerTest
    configAppLoggerFail
    "Test read logger from config must fail"

testEqual :: Eq b => (a -> b) -> a -> b -> String -> IO ()
testEqual f i o s =
  putStrLn $
  if f i == o
    then s ++ ": OK"
    else s ++ ": FAIL!"

testRight :: (a -> Either b c) -> a -> String -> IO ()
testRight f a s =
  putStrLn $
  case f a of
    Right _ -> s ++ ": OK"
    Left _ -> s ++ ": FAIL!"

testLeft :: (a -> Either b c) -> a -> String -> IO ()
testLeft f a s =
  putStrLn $
  case f a of
    Right _ -> s ++ ": FAIL!"
    Left _ -> s ++ ": OK"

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
  [ [("type", "stdio"), ("logLevel", "None")]
  , [("type", "file"), ("logLevel", "Debug"), ("filename", "echobot-test.log")]
  ]

loggerFail =
  [ [("type", "stdio"), ("logLevel", "None")]
  , [("type", ""), ("logLevel", "Debug"), ("filename", "echobot-test-fail.log")]
  ]

messengerC = [("type", "stdio"), ("showingKeyboardMessage", "keyboard")]

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

readLoggerTest ::
     [(String, String)] -> Either String (IO (LL.Logger IO String ()))
readLoggerTest c = snd $ BAC.interpret c PCF.initLoggingConfig

