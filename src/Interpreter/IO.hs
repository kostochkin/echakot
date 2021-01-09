module Interpreter.IO ( interpret ) where

import Language.IO (
          IOApi
        , IOApiF (
                      GetIOLine
                    , PutIOLine
                    , ReturnIO
                    , RunIO
                    , RawLog
                )
        )
import Control.Monad.Free ( Free( Pure, Free ) )

newtype RawString = RawString { getString :: String }

instance Show RawString where
    show = getString

interpret :: IOApi String Int () -> IO ()
interpret (Pure _)  = return ()
interpret (Free bf) = free bf where
    free (GetIOLine f)   = getLine >>= interpret . f
    free (PutIOLine s f) = putStrLn s >> interpret f
    free (ReturnIO io f) = io >>= interpret . f
    free (RunIO io f)    = io >> interpret f
    free (RawLog m f)    = print (fmap RawString m) >> interpret f

