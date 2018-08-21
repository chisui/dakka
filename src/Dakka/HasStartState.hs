{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE UndecidableInstances #-}
module Dakka.HasStartState
    ( HasStartState(..)
    ) where

import           "base" GHC.Generics (Generic (Rep), M1, U1 (..), to)


class HasStartState a where
    start :: a
    default start :: (Generic a, GHasStartState (Rep a)) => a
    start = to gstart

class GHasStartState (a :: * -> *) where
    gstart :: a b

instance GHasStartState U1 where
    gstart = U1

instance GHasStartState f => GHasStartState (M1 i c f) where
    gstart = gstart

