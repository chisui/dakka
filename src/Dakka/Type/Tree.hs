{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , UndecidableSuperClasses
           , TypeFamilies
           , UndecidableInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , FlexibleInstances
           , TypeSynonymInstances
           , GADTs
           , ConstraintKinds
           , DataKinds
           , PolyKinds
           , InstanceSigs
           , TypeApplications
           , TypeInType
           , FlexibleContexts
           , StandaloneDeriving
           , TemplateHaskell
           , PackageImports
           , QuasiQuotes
#-}
module Dakka.Type.Tree where

import "base" Data.List ( genericLength, genericIndex )
import "base" Data.Maybe ( fromJust )
import "base" Control.Arrow ( (&&&) )
import "base" Control.Monad ( guard )
import "base" Data.Functor.Identity ( Identity(..) )

import "base" Data.Typeable ( Typeable, typeOf, TypeRep, cast )
import "base" Data.Kind

import "containers" Data.Tree ( Tree(..) )

import Dakka.Constraints
import Dakka.Convert
import Dakka.Type.Path

-- ------------ --
--  Typed Tree  --
-- ------------ --

data TypedTree (cs :: [* -> Constraint]) (f :: k -> *) (t :: Tree k) where
    TNode :: (f a `ImplementsAll` cs) => f a -> TypedForest cs f c -> TypedTree cs f ('Node a c)

data TypedForest (cs :: [* -> Constraint]) (f :: k -> *) (ts :: [Tree k]) where
    TNil  :: TypedForest cs f '[]
    TCons :: [TypedTree cs f a] -> TypedForest cs f as -> TypedForest cs f (a ': as)

-- -------------------- --
--  demoted Typed Tree  --
-- -------------------- --

data HomogenousMultiTree (cs :: [* -> Constraint])
    = (Typeable :∈ cs) => HMTNode (ConstrainedDynamic cs) [(TypeRep, [HomogenousMultiTree cs])]
  deriving Typeable

{-
demoteTree :: (Typeable :∈ cs) => TypedTree cs f t -> HomogenousMultiTree cs
demoteTree (TNode n f) = HMTNode (CDyn n) (demoteForest f)

demoteForest :: (Typeable :∈ cs) => TypedForest cs f t -> [(TypeRep, [HomogenousMultiTree cs])]
demoteForest TNil = []
demoteForest (TCons l ls) = demoteHead l ++ demoteForest ls
  where
    demoteHead :: (Typeable :∈ cs) => [TypedTree cs f k] -> [(TypeRep, [HomogenousMultiTree cs])]
    demoteHead [] = []
    demoteHead l@((TNode n _) : _) = [(typeOf n, fmap demoteTree l)]

instance (Typeable :∈ cs) => Convertible (TypedTree cs f t) (HomogenousMultiTree cs) where
    convert = demoteTree
-}

-- ----------- --
--  Selecting  --
-- ----------- --

selectDemoted :: (Typeable :∈ cs) => Path (TypeRep, Word) -> HomogenousMultiTree cs -> Maybe (ConstrainedDynamic cs)
selectDemoted p t = do
    (HMTNode n _) <- selectDemotedSubTree p t
    Just n

selectDemotedSubTree :: (Typeable :∈ cs) => Path (TypeRep, Word) -> HomogenousMultiTree cs -> Maybe (HomogenousMultiTree cs)
selectDemotedSubTree Root t = Just t
selectDemotedSubTree (as :/ (t, n)) hmt = do
    (HMTNode _ f) <- selectDemotedSubTree as hmt
    l <- lookup t f
    guard (genericLength l > n)
    Just $ genericIndex l n

type family Select (p :: Path k) (t :: Tree k) :: k where
    Select p t = Root (SelectSubTree p t)
type family Root (t :: Tree k) :: k where
    Root ('Node n f) = n
type family SubTrees (t :: Tree k) :: [Tree k] where
    SubTrees ('Node n f) = f
type family SelectSubTree (p :: Path k) (t :: Tree k) :: Tree k where
    SelectSubTree 'Root      t = t
    SelectSubTree (as ':/ a) t = Lookup a (SubTrees (SelectSubTree as t)) 
type family Lookup (a :: k) (l :: [Tree k]) :: Tree k where
    Lookup a ('Node a f ': l) = 'Node a f
    Lookup a (t ': l)         = Lookup a l

select :: (SelectSubTreeC p t) => (Typeable (f (Select p t)), Typeable :∈ cs) => IndexedPath p -> TypedTree cs f t -> Maybe (f (Select p t))
select p t = root <$> selectSubTree p t

root :: TypedTree cs f t -> f (Root t)
root (TNode n _) = n

class SelectSubTreeC (p :: Path *) (t :: Tree *) where
    selectSubTree :: IndexedPath p -> TypedTree cs f t -> Maybe (TypedTree cs f (SelectSubTree p t))

instance SelectSubTreeC 'Root t where
    selectSubTree _ = Just

instance ( SelectSubTreeC as t
         , LookupByRoot a (SubTrees (SelectSubTree as t))
         , SelectSubTree (as ':/ a) t ~ Lookup a (SubTrees (SelectSubTree as t))
         ) => SelectSubTreeC (as ':/ a) t where
    selectSubTree (as :// a) t = lookupByRoot (ref @a a) . subTrees =<< selectSubTree as t

class LookupByRoot (a :: k) (l :: [Tree k]) where
    lookupByRoot :: IndexedRef a -> TypedForest cs f l -> Maybe (TypedTree cs f (Lookup a l))

instance {-# OVERLAPPING #-} LookupByRoot a ('Node a f ': as) where
    lookupByRoot r (TCons l _)
        | fromEnum r < length l = Just (l !! fromEnum r)
        | otherwise             = Nothing

instance {-# OVERLAPPABLE #-} (LookupByRoot a as, Lookup a (b ': as) ~ Lookup a as) => LookupByRoot a (b ': as) where
    lookupByRoot r (TCons _ as) = lookupByRoot r as

subTrees :: TypedTree cs f t -> TypedForest cs f (SubTrees t)
subTrees (TNode _ f) = f

-- ------------------- --
-- Stupid inline Tests --
-- ------------------- --


data A = CA String deriving (Eq, Show, Typeable)
data B = CB String deriving (Eq, Show, Typeable)
data C = CC String deriving (Eq, Show, Typeable)
data D = CD String deriving (Eq, Show, Typeable)
data E = CE String deriving (Eq, Show, Typeable)

t = TNode @Identity @_ @'[Typeable] (Identity ()) (
      TCons
          [ TNode (Identity $ CA "a") (
              TCons [] TNil)
          , TNode (Identity $ CA "b") (
              TCons
                  [ TNode (Identity $ CB "a.b") TNil]
              TNil)
          ]
      (TCons
          [ TNode (Identity $ CC "c") (
              TCons [] TNil)
          ]
      TNil))
