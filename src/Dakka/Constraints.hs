{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Dakka.Constraints where

import           "base" Data.Kind     (Constraint)
import           "base" Data.Proxy    (Proxy (..))
import           "base" Data.Typeable (Typeable)

import           "binary" Data.Binary (Binary)


type family (e :: k) ∈ (l :: [k]) :: Constraint where
    e ∈ (e ': as) = ()
    e ∈ (a ': as) = e ∈ as

type family (e :: k) ∈! (l :: [k]) :: Constraint where
    e ∈! (e ': as) = e ∉ as
    e ∈! (a ': as) = e ∈ as

type family (e :: k) ∉ (l :: [k]) :: Constraint where
    e ∉ (a ': as) = (e ≠ a, e ∉ as)
    e ∉ '[] = ()

type family (l0 :: [k]) ⊆ (l1 :: [k]) :: Constraint where
    l         ⊆ l = ()
    '[]       ⊆ l = ()
    (a ': as) ⊆ l = (a ∈ l, as ⊆ l)

type (a :: k) ≠ (b :: k) = (a `EqT` b) ~ 'False


type family (a :: k) `EqT` (b :: k) :: Bool where
    a `EqT` a = 'True
    a `EqT` b = 'False


type family (a :: k) `ImplementsAll` (c :: [k -> Constraint]) :: Constraint where
    a `ImplementsAll` (c ': cs) = (c a, a `ImplementsAll` cs)
    a `ImplementsAll` '[] = ()

class ImplementedByAll (c :: k -> Constraint) (l :: [k]) where
    applyT :: Applicative f => (forall a. c a => Proxy a -> f ()) -> f ()

instance ImplementedByAll c '[] where
    applyT _ = pure ()
instance (c e, ImplementedByAll c es) => ImplementedByAll c (e ': es) where
    applyT f = f (Proxy @e) *> applyT @_  @c @es f


type RichData  a = (Typeable a, Eq  a, Show  a, Binary a)


class Noop a where
    noop :: a
instance Noop () where
    noop = ()
instance (Noop a, Applicative f) => Noop (f a) where
    noop = pure noop

