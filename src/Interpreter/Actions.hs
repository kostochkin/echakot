module Interpreter.Actions (
        strToAction
    ) where

import Language.Bot ( Action(Help, TellRepeat, ModifyRepeat, Echo) )
import Text.Read ( readMaybe )
import Control.Monad ( guard )

strToAction :: (Read i, Eq i) => String -> [i] -> Action i
strToAction s k = case words s of
                    ["/help"]       -> Help
                    ["/repeat"]     -> TellRepeat
                    ["/repeat", si] -> maybe Echo ModifyRepeat $ f si
                    _               -> Echo
    where
        f si = do
            x <- readMaybe si
            guard (x `elem` k)
            return x
