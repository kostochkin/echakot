module Translator.Messenger.StdIO
  ( translate
  , IOApiLang
  ) where

import Config.App (App, helpReply, keyboard, keyboardMessage, repeatReply)
import Control.Monad.Free (Free(Free, Pure))
import Data.List.Split
import Interpreter.Actions (strToAction)
import Language.Bot (BotApi, BotApiF(..))
import Language.IO
  ( IOApi
  , getIOLine
  , getState
  , putIOLine
  , putState
  , rawLog
  , replicate
  )
import Prelude hiding (replicate)

type IOApiLang = IOApi String Int

translate :: App Int a -> BotApi String Int () -> IOApiLang ()
translate _ (Pure x) = Pure x
translate a (Free bf) = free bf
  where
    free (EchoMessage str f) = putIOLine str >> translate a f
    free (GetMessages f) = getIOLine >>= translate a . f . splitOn ","
    free (SelectAction str f) =
      return (strToAction str (map fst $ keyboard a)) >>= translate a . f
    free (ShowKeyboard f) = do
      putIOLine (keyboardMessage a)
      mapM_ (putIOLine . ("/repeat " ++)) $ map snd $ keyboard a
      translate a f
    free (SetRepeats i f) = putState i >> translate a f
    free (GetCurrentRepeats f) = getState >>= translate a . f
    free (TellCurrentRepeats f) = do
      i <- getState
      putIOLine $ repeatReply a ++ ": " ++ show i
      translate a f
    free (ShowHelp f) = putIOLine (helpReply a) >> translate a f
    free (BotLog m f) = rawLog m >> translate a f
    free (RunTimes i p f) = replicate i (translate a p) >> translate a f
