{-# LANGUAGE PackageImports #-}
module Dakka.RemoteTableState
    ( RemoteTableState(..)
    ) where

import           "distributed-static" Control.Distributed.Static (RemoteTable)
import           "transformers" Control.Monad.Trans.State        (State)

class Monad m => RemoteTableState m where
    liftTableState :: State RemoteTable a -> m a


