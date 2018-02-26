{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , UndecidableSuperClasses
           , TypeFamilies
           , TypeFamilyDependencies
           , UndecidableInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , FlexibleInstances
           , TypeSynonymInstances
           , GADTs
           , DataKinds
           , PolyKinds
           , InstanceSigs
           , TypeInType
           , FlexibleContexts
           , StandaloneDeriving
           , TemplateHaskell
           , QuasiQuotes
#-}
module Dakka.MixedSingleton where

import Data.Maybe
import Data.List
import Data.List.NonEmpty
import Data.Dynamic

import Data.Typeable

import Data.Kind

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.List hiding ( All )
import Data.Singletons.Prelude.List.NonEmpty
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Eq

import Data.Promotion.Prelude.Bool
import Data.Promotion.Prelude.Maybe
import Data.Promotion.Prelude.List hiding ( All )
import Data.Promotion.Prelude.List.NonEmpty
import Data.Promotion.Prelude.Function


data SubTrees (l :: [Tree k]) where
  MNil  :: SubTrees '[]
  (:::) :: MixedTree a -> SubTrees as -> SubTrees (a ': as)
infixr 4 :::
instance Show (SubTrees '[]) where
  show MNil = "MNil"
instance (Show (MixedTree a), Show (SubTrees as)) => Show (SubTrees (a ': as)) where
  show (t ::: s) = "(" ++ show t ++ ") ::: " ++ show s

data Tree a where
  (:->) :: a -> [Tree a] -> Tree a
deriving instance Show a => Show (Tree a)

data MixedTree (t :: Tree k) where
  (:|->) :: a -> SubTrees c -> MixedTree (a ':-> c)
leaf a = a :|-> MNil

instance (Show a, Show (SubTrees c)) => Show (MixedTree (a ':-> c)) where
  show (a :|-> c) = show a ++ " :|-> (" ++ show c ++ ")"

class Lifted t where
  type Lowered t
  lower :: t -> Lowered t

instance Lifted (SubTrees ('[] :: [Tree k])) where
  type Lowered (SubTrees ('[] :: [Tree k])) = [Tree Dynamic]
  lower MNil = []
instance (Lifted (MixedTree a), Lowered (MixedTree a) ~ Tree Dynamic, Lifted (SubTrees as), Lowered (SubTrees as) ~ [Tree Dynamic]) => Lifted (SubTrees (a ': as)) where
  type Lowered (SubTrees (a ': as)) = [Tree Dynamic]
  lower (a ::: as) = lower a : lower as
instance (Lifted (SubTrees c), Typeable a, Lowered (SubTrees c) ~ [Tree Dynamic]) => Lifted (MixedTree (a ':-> c)) where
  type Lowered (MixedTree (a ':-> c)) = Tree Dynamic
  lower (a :|-> b) = toDyn a :-> lower b

t0 = leaf 1
t1 = 1 :|-> (leaf 'c' ::: MNil)
t2 = 1 :|-> ( leaf 'c'
          ::: ((1,2) :|-> (("x" :|-> MNil) ::: MNil))
          ::: MNil)

