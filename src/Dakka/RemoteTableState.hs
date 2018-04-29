{-# LANGUAGE PackageImports #-}
module Dakka.RemoteTableState
    ( RemoteTableState(..)
    ) where

import "transformers" Control.Monad.Trans.State ( State )
import "distributed-static" Control.Distributed.Static ( RemoteTable )

class Monad m => RemoteTableState m where
    liftTableState :: State RemoteTable a -> m a


