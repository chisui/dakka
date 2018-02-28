{-# LANGUAGE TypeFamilies
           , PolyKinds
           , KindSignatures
           , ConstraintKinds
           , DataKinds
           , TypeOperators
           , PackageImports
#-}
module Dakka.Constraints where

import "base" Data.Kind ( Constraint )


type family (e :: k) :∈ (l :: [k]) :: Constraint where
    e :∈ (e ': as) = ()
    e :∈ (a ': as) = e :∈ as

type family (e :: k) :∈! (l :: [k]) :: Constraint where
    e :∈! (e ': as) = e :∉ as
    e :∈! (a ': as) = e :∈ as

type family (e :: k) :∉ (l :: [k]) :: Constraint where
    e :∉ (a ': as) = (e :≠ a, e :∉ as)
    e :∉ '[] = ()

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
