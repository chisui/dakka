{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , TypeFamilies
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
module Hakka.Tree where

import Data.Maybe
import Data.List
import Data.List.NonEmpty

import Data.Typeable

import Data.Kind

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.List.NonEmpty
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Eq

import Data.Promotion.Prelude.Bool
import Data.Promotion.Prelude.Maybe
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.List.NonEmpty
import Data.Promotion.Prelude.Function


$(singletons
  [d|
    
    data TTree a = a :-> [TTree a]
      deriving (Eq, Show, Read)

    class Predicate p a where
      test :: p -> a -> Bool  
    data TPath p (a :: *) = TPath (NonEmpty p)

    tRoot (r :-> _) = r
    tChildren (_ :-> cs) = cs

    tSubTree :: Predicate p a => p -> [TTree a] -> Maybe (TTree a)
    tSubTree p = find (test p . tRoot)

    tSelect :: Predicate p a => TPath p a -> TTree a -> Maybe a
    tSelect (TPath (a :| as)) (r :-> cs) = if test a r
        then tSelect' r as cs
        else Nothing
      where
        tSelect' :: Predicate q b => b -> [q] ->  [TTree b] -> Maybe b
        tSelect' r' []     _  = Just r'
        tSelect' r' (p:ps) ts = case tSubTree p ts of
            Nothing                -> Nothing
            (Just (r'' :-> cs')) -> tSelect' r'' ps cs'
  
  |])

type Leaf a = a ':-> '[]
leaf a = a :%-> SNil
(.:) = SCons
a .:. b = SCons a (SCons b SNil)

-- ------------------- --
-- Stupid inline Tests --
-- ------------------- --


data A = CA String deriving (Eq, Show, Typeable)
data instance Sing A where
  SCA :: String -> Sing A

data B = CB String deriving (Eq, Show, Typeable)
data instance Sing B where
  SCB :: String -> Sing B

data C = CC String deriving (Eq, Show, Typeable)
data instance Sing C where
  SCC :: String -> Sing C

data D = CD String deriving (Eq, Show, Typeable)
data instance Sing D where
  SCD :: String -> Sing D

data E = CE String deriving (Eq, Show, Typeable)
data instance Sing E where
  SCE :: String -> Sing E

{-
t :: STTree (
    A :-> '[
        B :-> '[
            Leaf D,
            Leaf E], 
        Leaf C])
-}
t = SCA "0" :%-> 
        (   SCB "0.0" :%-> 
            (   leaf (SCD "0.0.0") 
            .:. leaf (SCE "0.0.1")
            )
        .:. leaf (SCC "0.1")
        ) 


