import Bot.FsdEcho as FE
import Interpreters.BotApiTest as BAT
import Control.Monad (forever)


main :: IO ()
main = do
    putStrLn ""
    testEqual botApiTest botApiInput botApiOutput "Test Bot API"


botApiTest = uncurry $ BAT.interpret (forever FE.botStep)
botApiInput = ((1, ["/help", "111", "/repeat", "222", "/repeat 2", "/repeat", "333", "/repeat 3", "444", "/repeat -1", "555", "/repeat 10", "666"]), [1 .. 5])
botApiOutput = ["help", "111", "1", "keyboard", "222", "2", "keyboard", "333", "333", "444", "444", "444", "/repeat -1", "/repeat -1", "/repeat -1", "555", "555", "555", "/repeat 10", "/repeat 10", "/repeat 10", "666", "666", "666"]

testEqual :: Eq b => (a -> b) -> a -> b -> String -> IO ()
testEqual f i o s = putStrLn $ if f i == o then s ++ ": OK" else s ++ ": FAIL!"
