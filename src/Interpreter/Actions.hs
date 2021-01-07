module Interpreter.Actions where

import Language.Bot

strToAction :: (Read i, Eq i) => String -> [i] -> Action i
strToAction s k = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ try (reads si)
                    _               -> Echo
    where
        try [(x, "")] | x `elem` k = Just x
        try _                      = Nothing
