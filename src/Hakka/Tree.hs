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

import Data.Kind

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.List.NonEmpty
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Eq

import Data.Promotion.Prelude
import Data.Promotion.Prelude.Maybe


$(singletons [d|
    
    data TTree a = TTree a [TTree a]
      deriving (Eq, Show, Read)

    data TPath a = TPath (NonEmpty a)

    tRoot (TTree r _) = r
    tChildren (TTree _ cs) = cs

    tSubTree :: Eq a => a -> [TTree a] -> Maybe (TTree a)
    tSubTree a = find ((== a) . tRoot)

    tSelect :: Eq a => TPath a -> TTree a -> Maybe a
    tSelect (TPath (a :| as)) (TTree r cs) = if a == r
        then tSelect' r as cs
        else Nothing
      where
        tSelect' :: Eq b => b -> [b] ->  [TTree b] -> Maybe b
        tSelect' r' []     _  = Just r'
        tSelect' r' (p:ps) ts = case tSubTree p ts of
            Nothing                -> Nothing
            (Just (TTree r'' cs')) -> tSelect' r'' ps cs'
  
  |])

type (:->:) = 'TTree
(.->.) = STTree

type Leaf a = 'TTree a '[]
leaf a = STTree a SNil

-- ------------------- --
-- Stupid inline Tests --
-- ------------------- --


data A = CA String deriving (Eq, Show)
data instance Sing A where
  SCA :: String -> Sing A

data B = CB String deriving (Eq, Show)
data instance Sing B where
  SCB :: String -> Sing B

data C = CC String deriving (Eq, Show)
data instance Sing C where
  SCC :: String -> Sing C

data D = CD String deriving (Eq, Show)
data instance Sing D where
  SCD :: String -> Sing D

data E = CE String deriving (Eq, Show)
data instance Sing E where
  SCE :: String -> Sing E

{-
t :: STTree (
    A :->: '[
        B :->: '[
            Leaf D,
            Leaf E], 
        Leaf C])
-}
t = SCA "0" .->. (
        SCB "0.0" .->. (
            leaf (SCD "0.0.0") `SCons`
            leaf (SCE "0.0.1") `SCons`
            SNil) `SCons`
        leaf (SCC "0.1") `SCons`
        SNil) 


