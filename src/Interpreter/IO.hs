module Interpreter.IO ( interpret ) where

import Language.IO
import Control.Monad.Free

interpret :: IOApi String Int () -> IO ()
interpret (Pure _)  = return ()
interpret (Free bf) = free bf where
    free (GetIOLine f)   = getLine >>= interpret . f
    free (PutIOLine s f) = putStrLn s >> interpret f
    free (ReturnIO io f) = io >>= interpret . f
    free (RunIO io f)    = io >> interpret f
    free (RawLog m f)    = print m >> interpret f

