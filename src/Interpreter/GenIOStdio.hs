module Interpreter.GenIOStdio
  ( interpret
  ) where

import Config.Messenger.StdIO (StdioMessenger, state)
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar, MVar)
import Control.Monad (replicateM_)
import Control.Monad.Free (Free(Free, Pure))
import Language.GeneralIO (GenIO, GenIOF(..))
import Log.Logger (Logger, (|>), (*>>), inlineLogDebug)
--import Log.Message (LogMessage)
import System.IO (hFlush, stdout)

interpret :: StdioMessenger -> Logger IO String () -> GenIO String String Int () -> IO ()
interpret _ _ (Pure _) = return ()
interpret s l (Free f') = free f'
  where
    free (IOMessage "" f) = do
        putStr ">> "
        hFlush stdout
        x <- getLine
        interpret s l (f x)
    free (IOMessage str f) = do
        putStrLn str
        interpret s l (f "")
    free (IOState g f) = do
        v <- takeMVar (state s)
        putMVar (state s) (g v)
        interpret s l (f v)
    free (LogGenIO m f) = m |> l *>> interpret s l f
    free (RepeatIO i p f) = do
        k <- newMVar 1 :: IO (MVar Int)
        replicateM_ i ( do
            k' <- takeMVar k
            inlineLogDebug ("IO: Repeat " ++ show k' ++ "/" ++ show i) l *>> return ()
            putMVar k (succ k')
            interpret s l p)
        interpret s l f

