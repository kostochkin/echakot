module Config where

loggers :: [[(String, String)]]
loggers = [ [("type", "stdio"),("logLevel", "None")]
          , [("type", "file"),("logLevel", "Debug"),("filename", "echobot.log")]]

messengerC :: [(String, String)]
messengerC = [ ("type", "stdio")
             , ("showingKeyboardMessage", "Please, enter one of the following commands to modify repeats")]

configApp :: [(String, String)]
configApp = [("loggers", show loggers), ("messenger", show messengerC), ("botGeneral", show botGeneral)]

botGeneral :: [(String, String)]
botGeneral = [ ("repeats1", "1w")
             , ("repeatsMessage"
             , "Current repeats")
             , ("helpMessage", "Hi, I'm an echo bot. My name is Echakot. I know these commands: /repeat, /help. Do you know that?")]


