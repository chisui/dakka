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
    
    data Tree a = Tree a [Tree a]
      deriving (Eq, Show, Read)

    type Path a = NonEmpty a

    root (Tree r _) = r
    children (Tree _ cs) = cs

    subTree :: Eq a => a -> [Tree a] -> Maybe (Tree a)
    subTree a = find ((== a) . root)

    select :: Eq a => Path a -> Tree a -> Maybe a
    select (a :| as) (Tree r cs) = if a == r
        then select' r as cs
        else Nothing
      where
        select' :: Eq b => b -> [b] ->  [Tree b] -> Maybe b
        select' r' []     _  = Just r'
        select' r' (p:ps) ts = case subTree p ts of
            Nothing               -> Nothing
            (Just (Tree r'' cs')) -> select' r'' ps cs'
  
  |])





