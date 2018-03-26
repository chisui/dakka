{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , DeriveGeneric
           , DeriveFunctor
           , DeriveTraversable
           , DeriveFoldable
           , DefaultSignatures
           , FunctionalDependencies
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
           , GeneralizedNewtypeDeriving
           , QuasiQuotes
           , TypeApplications
           , PackageImports
#-}
-- | Typesafe Paths with type unsafe indices.
module Dakka.Type.Path
    ( Path(..)
    , Tip
    , IndexedPath(..)
    , IndexedRef(..)
    , ref
    , AllSegmentsImplement
    ) where

import           "base" Data.Functor.Classes 
                          ( Show1( liftShowsPrec )
                          , Eq1( liftEq )
                          , Ord1( liftCompare )
                          , Read1( liftReadPrec, liftReadListPrec )
                          , liftReadListPrecDefault
                          )
import           "base" GHC.Exts ( IsList( Item, fromList, toList ) )
import           "base" Control.Monad ( ap )
import qualified "base" Data.Foldable as F
import           "base" GHC.Generics ( Generic )
import           "base" Data.Kind
import           "base" Data.Typeable ( Typeable, typeOf, Proxy, TypeRep )

import           Dakka.Convert

-- | A Path of type 'a'.
-- Used as promoted type in @IndexedPath@.
data Path a
    = Root a -- ^ the root of a path
    | Path a :/ a -- ^ a sub path
  deriving (Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)
infixl 5 :/

type family Tip (p :: Path k) :: k where
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

instance IsList (Path a) where
    type Item (Path a) = a
    toList = F.toList
    fromList (a:as) = F.foldr' (<>) (Root a) $ fmap pure as

instance Show a => Show (Path a) where
    showsPrec = liftShowsPrec showsPrec showList

instance Show1 Path where
    liftShowsPrec s _ i p str = '/' : foldl showSegment "" p ++ str
      where showSegment str e = str ++ s (i-1) e "" ++ "/"

instance Eq1 Path where
    liftEq eq a b = liftEq eq (toList a) (toList b)

instance Ord1 Path where
    liftCompare ord a b = liftCompare ord (toList a) (toList b)


-- | A typesafe path with indices.
data IndexedPath (p :: Path *) where
    -- | the root of a path.
    IRoot :: IndexedPath ('Root a)
    -- | a sub path.
    -- Since 'a' has to be 'Typeable' we don't need to story anything and 'a' can be a phantom type variable.
    (://) :: IndexedPath as -> Word -> IndexedPath (as ':/ a)
infixl 5 ://
deriving instance Typeable (IndexedPath p)

type family AllSegmentsImplement (p :: Path k) (c :: k -> Constraint) :: Constraint where
    AllSegmentsImplement ('Root a)  c = (c a)
    AllSegmentsImplement (as ':/ a) c = (c a, AllSegmentsImplement as c)

-- | Retrieve the type of the tip of a path.
typeOfTip :: forall p. Typeable (Tip p) => IndexedPath p -> TypeRep
typeOfTip _ = typeOf @(Tip p) undefined

-- | Retrieve the demoted tip.
demoteTip :: Typeable (Tip p) => IndexedPath p -> (TypeRep, Word)
demoteTip p@(_ :// i) = (typeOfTip p, i)

-- | Demote a path.
demotePath :: p `AllSegmentsImplement` Typeable => IndexedPath p -> Path (TypeRep, Word)
demotePath p@IRoot = Root $ demoteTip p
demotePath p@(p' :// _) = demotePath p' :/ demoteTip p

instance (p `AllSegmentsImplement` Typeable) => Convertible (IndexedPath p) (Path (TypeRep, Word)) where
    convert = demotePath

instance (p `AllSegmentsImplement` Typeable) => Show (IndexedPath p) where
    showsPrec i = liftShowsPrec showDemoted undefined i . demotePath
      where showDemoted _ (t, i) str = str ++ show t ++ '!' : show i

-- | Represents a Segement of an 'IndexedPath'.
newtype IndexedRef a
    = IR Word
  deriving (Eq, Ord, Functor, Enum)

ref :: Typeable a => Word -> IndexedRef a
ref = IR

