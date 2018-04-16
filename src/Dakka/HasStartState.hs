{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Dakka.HasStartState
    ( HasStartState(..)
    ) where

import "base" GHC.Generics ( Generic(Rep), to, U1(..), M1 )


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

