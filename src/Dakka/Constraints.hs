{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Dakka.Constraints where

import "base" Data.Kind -- We need *, that can't be expressed in an import filter
import "base" Data.Typeable ( Typeable, cast )
import "base" Data.Functor.Classes ( Eq1(..), Show1, Eq2(..) )
import "base" Unsafe.Coerce ( unsafeCoerce ) -- used for 'downCast'
import "base" Data.Function ( on )

type family (e :: k) :∈ (l :: [k]) :: Constraint where
    e :∈ (e ': as) = ()
    e :∈ (a ': as) = e :∈ as

type family (e :: k) :∈! (l :: [k]) :: Constraint where
    e :∈! (e ': as) = e :∉ as
    e :∈! (a ': as) = e :∈ as

type family (e :: k) :∉ (l :: [k]) :: Constraint where
    e :∉ (a ': as) = (e :≠ a, e :∉ as)
    e :∉ '[] = ()

type family (l0 :: [k]) :⊆ (l1 :: [k]) :: Constraint where
    '[]       :⊆ l = ()
    (a ': as) :⊆ l = (a :∈ l, as :⊆ l)

type (:.) (g :: b -> c) (f :: a -> b) (t :: a) = g (f t)

type (a :: k) :≠ (b :: k) = (a `EqT` b) ~ 'False


type family (a :: k) `EqT` (b :: k) :: Bool where
    a `EqT` a = 'True
    a `EqT` b = 'False


type family (a :: k) `ImplementsAll` (c :: [k -> Constraint]) :: Constraint where
    a `ImplementsAll` (c ': cs) = (c a, a `ImplementsAll` cs)
    a `ImplementsAll` '[] = ()

type family (c :: k -> Constraint) `ImplementedByAll` (l :: [k]) :: Constraint where
    c `ImplementedByAll` (a ': as) = (c a, c `ImplementedByAll` as)
    c `ImplementedByAll` '[] = ()


type RichData  a = (Typeable a, Eq  a, Show  a)
type RichData1 a = (Typeable a, Eq1 a, Show1 a)

(=~=) :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
a =~= b = Just a == cast b

(=~~=) :: (Typeable (f a), Typeable (g b), Eq1 f) => f a -> g b -> Bool
a =~~= b = Just a `eq'` cast b
  where
    eq' :: Eq1 f => Maybe (f a) -> Maybe (f a) -> Bool
    eq' = liftEq (liftEq (const $ const False))

(=~~~=) :: (Typeable (f a b), Typeable (g c d), Eq2 f) => f a b -> g c d -> Bool
a =~~~= b = Just a `eq'` cast b
  where
    eq' :: Eq2 f => Maybe (f a b) -> Maybe (f a b) -> Bool
    eq' = liftEq (liftEq2 no no)
    no _ _ = False

data ConstrainedDynamic (cs :: [* -> Constraint])
    = forall a. (a `ImplementsAll` cs) => CDyn a

liftConstrained :: (a :⊆ b) => (ConstrainedDynamic a -> c) -> ConstrainedDynamic b -> c
liftConstrained f = f . downCast 

downCast :: (b :⊆ a) => ConstrainedDynamic a -> ConstrainedDynamic b
downCast = unsafeCoerce -- removing constrains is nothing more than casting to it

instance ('[Typeable, Eq] :⊆ cs) => Eq (ConstrainedDynamic cs) where
    (==) = eq `on` downCast 
      where
        eq :: ConstrainedDynamic '[Typeable, Eq] -> ConstrainedDynamic '[Typeable, Eq] -> Bool
        eq (CDyn a) (CDyn b) = a =~= b

instance (Show :∈ cs) => Show (ConstrainedDynamic cs) where
    show = ("ConstrainedDynamic " ++) . liftConstrained @'[Show] (\ (CDyn a) -> show a)

type RichDynamic = ConstrainedDynamic '[Typeable, Eq, Show]

class ShowShadow (f :: k -> *) where
    showsShadow :: Int -> f a -> ShowS
    default showsShadow :: Show (f a) => Int -> f a -> ShowS
    showsShadow = showsPrec

class ShowShadow2 (f :: k0 -> k1 -> *) where
    showsShadow2 ::  Int -> f a b -> ShowS
    default showsShadow2 :: Show (f a b) => Int -> f a b -> ShowS
    showsShadow2 = showsPrec


class EqShadow (f :: k -> *) where
    eqShadow :: f a -> f a -> Bool 
    default eqShadow :: Eq (f a) => f a -> f a -> Bool 
    eqShadow = (==) 

class EqShadow2 (f :: k0 -> k1 -> *) where
    eqShadow2 ::  f a b -> f a b -> Bool
    default eqShadow2 :: Eq (f a b) => f a b -> f a b -> Bool 
    eqShadow2 = (==) 

