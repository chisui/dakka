{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , TypeFamilies
           , UndecidableInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , FlexibleInstances
           , TypeSynonymInstances
           , GADTs
           , FlexibleContexts
           , StandaloneDeriving
#-}
module Hakka.Tree where

import Data.Singleton.List
import Data.Proxy ( Proxy(..) )
import Data.Typeable ( Typeable(..), cast )
import Data.Maybe ( fromJust )
import Data.Foldable ( asum )
import Control.Applicative ( Alternative(..), liftA2 )


-- Typed Tree --

-- | A Rose Tree that keeps track of its kind.
data a :#: b where
  (:#:) :: a -> b
deriving instance (Show a, Show b) => Show (a :#: b)
deriving instance (Typeable a, Typeable b) => Typeable (a :#: b)
infixr 6 :#:

-- | Alias for Tree to create Leafs
type Leaf a = a :#: Nil

leaf :: a -> Leaf a
leaf a = a :#: Nil

-- Typed Node List --

-- | Marker class that is only implemented by 'Nil' and ':+:'
-- TODO replace with PolyKinds, TypeFamily or something like that.
class Typeable a => TypedNodeList a where

-- | Singleton for empty List
data Nil = Nil deriving (Eq, Show, Typeable)
instance TypedNodeList Nil where

-- | Cons for List
data a :+: b = a :+: b deriving (Eq, Show, Typeable)
infixr 5 :+:
instance (Typeable a, TypedNodeList b, TypedNodeList c) => TypedNodeList (a :#: c :+: b) where


-- Typed Path in Tree --
{-
data a :/: b = Proxy a :/: b deriving (Eq, Show, Typeable)
infixr 5 :/:

data NotASubTree deriving Typeable
type family SubTree a0 a1 where
  SubTree (Proxy a)  ((a :#: b) :+: c) = a :#: b
  SubTree (a :/: as) ((a :#: b) :+: c) = a :#: b
  SubTree  a         (    x     :+: t) = SubTree a t
  SubTree  a         Nil               = NotASubTree

subTree :: forall a b. (Typeable a, Typeable b, Typeable (SubTree a b)) => a -> b -> SubTree a b
subTree a b = fromJust $ asum 
    [ cast =<< liftA2 subTreeProxy (cast a) (cast b)
    , cast =<< liftA2 subTreePath  (cast a) (cast b)
    , cast =<< liftA2 subTreeRec   (cast a) (cast b)
    ]
  where
    subTreeProxy :: (Typeable a, Typeable b, Typeable c) => (Proxy a) -> ((a :#: b) :+: c) -> (a :#: b)
    subTreeProxy = undefined
    subTreePath :: (Typeable a, Typeable as, Typeable b, Typeable c) => (a :/: as) -> ((a :#: b) :+: c) -> (a :#: b)
    subTreePath = undefined
    subTreeRec :: (Typeable a, Typeable b, Typeable c) => a -> (b :+: c) -> SubTree a c
    subTreeRec a (_ :+: c)= subTree a c

class Selectable a b where
  type Selects a b
  select :: a -> b -> Selects a b

instance Selectable (Proxy a) (a :#: b) where
  type Selects (Proxy a) (a :#: b) = a
  select _ (a :#: _) = a

instance (Typeable (SubTree as ts), Typeable as, Typeable ts, Selectable as (SubTree as ts)) => Selectable (a :/: as) (a :#: ts) where
  type Selects (a :/: as) (a :#: ts) = Selects as (SubTree as ts)
  select (_ :/: as) (_ :#: ts) = select as (subTree as ts)

-- Test Usage --

data A = A deriving (Eq, Show, Typeable)
_A = Proxy :: Proxy A
data B = B deriving (Eq, Show, Typeable)
_B = Proxy :: Proxy B
data C = C deriving (Eq, Show, Typeable)
_C = Proxy :: Proxy C
data D = D deriving (Eq, Show, Typeable)
_D = Proxy :: Proxy D
data E = E deriving (Eq, Show, Typeable)
_E = Proxy :: Proxy E
data F = F deriving (Eq, Show, Typeable)
_F = Proxy :: Proxy F


type T = A :#: (Leaf B :+: (C :#: (Leaf D :+: Nil)) :+: Nil)
t :: T
t =  A :#: (leaf B :+: (C :#: (leaf D :+: Nil)) :+: Nil)

selectA :: T -> A
selectA = select _A

selectB :: T -> C
selectB = select (_A :/: _C)
-}
