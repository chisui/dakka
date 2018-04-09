{-# LANGUAGE Safe #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Dakka.Path.Base where

import "base" Data.Functor.Classes 
                          ( Show1( liftShowsPrec )
                          , Eq1( liftEq )
                          , Ord1( liftCompare )
                          )
import "base" Control.Monad ( ap )
import "base" Data.Foldable ( toList )
import "base" GHC.Generics ( Generic )
import "base" Data.Typeable ( Typeable )

-- | A Path of type 'a'.
-- Used as promoted type in @IndexedPath@.
data Path a
    = Root a -- ^ the root of a path
    | Path a :/ a -- ^ a sub path
  deriving (Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)
infixl 5 :/

type family PRoot (p :: Path k) = (t :: k) where
    PRoot ('Root a)  = a
    PRoot (as ':/ a) = PRoot as

type family Tip (p :: Path k) = (t :: k) where
    Tip ('Root a)  = a
    Tip (as ':/ a) = a

type family Pop (p :: Path k) :: Path k where
    Pop (as ':/ a) = as

instance Semigroup (Path a) where
    p <> (Root a)  = p :/ a
    p <> (p' :/ a) = (p <> p') :/ a

instance Applicative Path where
    pure = Root
    (<*>) = ap

instance Monad Path where
    (Root a)  >>= f = f a
    (as :/ a) >>= f = (as >>= f) <> f a

instance Show a => Show (Path a) where
    showsPrec = liftShowsPrec showsPrec showList

instance Show1 Path where
    liftShowsPrec s _ i p st = '/' : foldl showSegment "" p ++ st
      where showSegment str e = str ++ s (i-1) e "" ++ "/"

instance Eq1 Path where
    liftEq eq a b = liftEq eq (toList a) (toList b)

instance Ord1 Path where
    liftCompare ord a b = liftCompare ord (toList a) (toList b)

