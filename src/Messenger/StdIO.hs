{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Messenger.StdIO
    ( StdIOMessenger
    , runStdIOMessenger
    , M.sendMessage
    , M.receiveMessage
    , M.createKeyboard
    ) where

import Messenger.Api as M
import Transport.StdIO as T

newtype StdIOMessenger a = StdIOMessenger (StdIO a) deriving (Functor, Applicative, Monad)

runStdIOMessenger (StdIOMessenger x) = T.runStdIO x

instance M.Api StdIOMessenger where
    sendMessage x = StdIOMessenger $ T.sendMessage $ M.getMessage x
    receiveMessage = StdIOMessenger $ fmap M.message $ T.waitMessage
    createKeyboard = StdIOMessenger $ undefined

