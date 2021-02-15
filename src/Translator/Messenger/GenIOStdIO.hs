module Translator.Messenger.GenIOStdIO
  ( translate
  , GenIOLang
  ) where

import Config.App (App, helpReply, keyboard, keyboardMessage, repeatReply)
import Control.Monad.Free (Free(Free, Pure))
import Data.List.Split
import Interpreter.Actions (strToAction)
import Language.Bot (BotApi, BotApiF(..))
import Language.GeneralIO
  ( GenIO
  , runIOMessage
  , getIOState
  , putIOState
  , logGenIO
  , repeatIO
  )
import Prelude hiding (replicate)

type GenIOLang = GenIO String String Int

translate :: App Int a -> BotApi String Int () -> GenIOLang ()
translate _ (Pure x) = Pure x
translate a (Free bf) = free bf
  where
    free (EchoMessage str f) = do
        _ <- runIOMessage str
        translate a f
    free (GetMessages f) = do
        ms <- runIOMessage ""
        translate a (f (splitOn "," ms))
    free (SelectAction str f) =
      return (strToAction str (map fst $ keyboard a)) >>= translate a . f
    free (ShowKeyboard f) = do
      _ <- runIOMessage (keyboardMessage a)
      mapM_ (runIOMessage . ("/repeat " ++)) $ map snd $ keyboard a
      translate a f
    free (SetRepeats i f) = putIOState i >> translate a f
    free (GetCurrentRepeats f) = getIOState >>= translate a . f
    free (TellCurrentRepeats f) = do
      i <- getIOState
      _ <- runIOMessage $ repeatReply a ++ ": " ++ show i
      translate a f
    free (ShowHelp f) = do
        _ <- runIOMessage (helpReply a)
        translate a f
    free (BotLog m f) = logGenIO m >> translate a f
    free (RunTimes i p f) = repeatIO i (translate a p) >> translate a f

