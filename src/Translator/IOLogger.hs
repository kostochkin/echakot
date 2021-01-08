module Translator.IOLogger where

import qualified Log.Message as LM
import qualified Log.Logger as LL
import Language.IO
import Control.Monad.Free
import Control.Monad

addLogging :: LL.Logger -> IOApi String Int () -> IOApi String Int ()
addLogging l f@(Pure x) = LL.maybeLog l f (ioLog >=> const f) (LM.logDebug ("IO finished: " ++ show x))
addLogging l (Free b)   = maybe (free b) (ioLog >=> const (free b)) (LL.maybeMessage l tryMessage b) where
    free (GetIOLine g)   = maybeLogValue getIOLine (LM.logInfo "line") >>= addLogging l . g
    free (PutIOLine s g) = putIOLine s >> addLogging l g
    free (ReturnIO io g) = maybeLogValue (returnIO io) (LM.logDebug "value") >>= addLogging l . g
    free (RunIO io g)    = runIO io >> addLogging l g
    free (RawLog s g)    = rawLog s >> addLogging l g
    maybeLogValue f m = LL.maybeLogValue l f (fmap (\x y -> ("Got " ++ x ++ ": " ++ show y)) m) ioLog


tryMessage :: (Show b, Show i) => IOApiF b i a -> Maybe (LM.LogMessage String)
tryMessage (RawLog _ _)    = Nothing
tryMessage (GetIOLine _)   = Just $ LM.logDebug "Getting line ... "
tryMessage (PutIOLine b _) = Just $ LM.logInfo $ "Writing line " ++ show b
tryMessage (RunIO _ _)     = Just $ LM.logDebug "Running raw IO ()"
tryMessage (ReturnIO _ _)  = Just $ LM.logDebug "Querying raw IO a"

