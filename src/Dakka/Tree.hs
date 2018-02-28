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
           , DataKinds
           , PolyKinds
           , InstanceSigs
           , TypeInType
           , FlexibleContexts
           , StandaloneDeriving
           , TemplateHaskell
           , QuasiQuotes
#-}
module Dakka.Tree where

import Data.Maybe
import Data.List
import Data.List.NonEmpty

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


$(singletons
  [d|
    
    data RoseTree a 
        = a :-> [RoseTree a]
      deriving (Eq, Show, Read)

    type TreePath a = NonEmpty (a -> Bool)

    root (r :-> _) = r
    children (_ :-> cs) = cs

    subTree :: (a -> Bool) -> [RoseTree a] -> Maybe (RoseTree a)
    subTree p = find (p . root)

    select :: TreePath a -> RoseTree a -> Maybe a
    select (a :| as) (r :-> cs) = if a r
        then select' r as cs
        else Nothing
      where
        select' :: b -> [b -> Bool] ->  [RoseTree b] -> Maybe b
        select' r' []     _  = Just r'
        select' r' (p:ps) ts = case subTree p ts of
            Nothing              -> Nothing
            (Just (r'' :-> cs')) -> select' r'' ps cs'
  
  |])


data TypedList (a :: [k]) where
  TNil  :: TypedList '[]
  (:::) :: a -> TypedList as -> TypedList (a ': as)
instance Show (TypedList '[]) where
  show TNil = "TypedList []"
instance (Show a, Show (TypedList as)) => Show (TypedList (a ': as)) where
  show (a ::: as) = "TypedList [" ++ show a ++ end
    where
      next = Data.List.drop 11 $ show as
      end  = if next == "]" then "]" else ", " ++ next

data TypedTree (a :: k) where
  (:|->) :: a -> TypedList c -> TypedTree (a ':-> Demoted c)

type family Demoted (l :: [k]) :: [j]
type instance Demoted '[] = '[]
type instance Demoted (TypedTree a ': as) = a ': Demoted as


-- ------------------- --
-- Stupid inline Tests --
-- ------------------- --


data A = CA String deriving (Eq, Show, Typeable)
data B = CB String deriving (Eq, Show, Typeable)
data C = CC String deriving (Eq, Show, Typeable)
data D = CD String deriving (Eq, Show, Typeable)
data E = CE String deriving (Eq, Show, Typeable)



