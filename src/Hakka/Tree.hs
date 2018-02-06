{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , TypeSynonymInstances
           , GADTs
           , StandaloneDeriving
#-}
module Hakka.Tree where




-- | A Rose Tree that keeps track of its kind.
data a :#: b where
  (:#:) :: TypedNodeList b => a -> b -> a :#: b
deriving instance (Show a, Show b) => Show (a :#: b)
infixr 6 :#:

-- | Alias for Tree to create Leafs
type Leaf a = a :#: Nil

leaf :: a -> Leaf a
leaf a = a :#: Nil

-- Typed Node List --

-- | Marker class that is only implemented by 'Nil' and ':+:'
-- TODO replace with PolyKinds, TypeFaimily or something like that.
class TypedNodeList a where

-- | Singleton for empty List
data Nil = Nil deriving Show
instance TypedNodeList Nil where

-- | Cons for List
data a :+: b = a :+: b deriving Show 
infixr 5 :+:
instance (TypedNodeList b, TypedNodeList c) => TypedNodeList (a :#: c :+: b) where


-- Test Usage --

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show
data D = D deriving Show
data E = E deriving Show
data F = F deriving Show

-- Type should be identical to Value. 
t :: A :#: (Leaf B :+: (C :#: (Leaf D :+: Nil)) :+: Nil)
t =  A :#: (leaf B :+: (C :#: (leaf D :+: Nil)) :+: Nil)

