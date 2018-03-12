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
           , QuasiQuotes
           , TypeApplications
           , PackageImports
#-}
-- | Typesafe Paths with type unsafe indices.
module Dakka.Type.Path
    ( Path(..)
    , IndexedPath(..)
    , (</>)
    , demotePath
    , IsPath(..)
    , CanAppend(..)
    , IndexedRef
    , ref
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
    = Root -- ^ the root of a path
    | Path a :/ a -- ^ a sub path
  deriving (Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)
infixl 5 :/

instance Semigroup (Path a) where
    p <> Root = p
    p <> (p' :/ a) = (p <> p') :/ a

instance Monoid (Path a) where
    mempty = Root

instance Applicative Path where
    pure = (Root :/)
    (<*>) = ap

instance Monad Path where
    Root      >>= _ = Root
    (as :/ a) >>= f = (as >>= f) <> f a

instance IsList (Path a) where
    type Item (Path a) = a
    toList = F.toList
    fromList = mconcat . fmap pure

instance Show a => Show (Path a) where
    showsPrec = liftShowsPrec showsPrec showList

instance Show1 Path where
    liftShowsPrec s _ i p str = str ++ '/' : foldl showSegment "" p
      where showSegment str e = str ++ s (i-1) e "" ++ "/"

instance Eq1 Path where
    liftEq eq a b = liftEq eq (toList a) (toList b)

instance Ord1 Path where
    liftCompare ord a b = liftCompare ord (toList a) (toList b)


-- | A typesafe path with indices.
data IndexedPath (p :: Path *) where
    -- | the root of a path.
    IRoot :: IndexedPath 'Root
    -- | a sub path.
    -- Since 'a' has to be 'Typeable' we don't need to story anything and 'a' can be a phantom type variable.
    (://) :: Typeable a => IndexedPath as -> Word -> IndexedPath (as ':/ a)
infixl 5 ://
deriving instance Typeable (IndexedPath p)

-- | Retrieve the type of the tip of a path.
typeOfTip :: forall a as. Typeable a => IndexedPath (as ':/ a) -> TypeRep
typeOfTip _ = typeOf @a undefined

-- | Retrieve the demoted tip.
demoteTip :: IndexedPath (as ':/ a) -> (TypeRep, Word)
demoteTip p@(_ :// i) = (typeOfTip p, i)

-- | Demote a path.
demotePath :: IndexedPath p -> Path (TypeRep, Word)
demotePath IRoot = Root
demotePath p@(p' :// _) = demotePath p' :/ demoteTip p

instance Convertible (IndexedPath p) (Path (TypeRep, Word)) where
    convert = demotePath

instance Show (IndexedPath p) where
    showsPrec i = liftShowsPrec showDemoted undefined i . demotePath
      where showDemoted _ (t, i) = (++ show t ++ '!' : show i)

-- | Represents a Segement of an 'IndexedPath'.
newtype IndexedRef a
    = IR Word
  deriving (Eq, Ord, Functor)

ref :: Typeable a => Word -> IndexedRef a
ref = IR

class IsPath a p | a -> p where
    toPath :: a -> p
    default toPath :: (a ~ p) => a -> p
    toPath = id

class CanAppend as a p | as a -> p where
    pappend :: as -> a -> p 

(</>) :: (IsPath a p, CanAppend p b c) => a -> b -> c 
as </> a = toPath as `pappend` a
infixl 5 </>

instance IsPath (Path a) (Path a) 
instance IsPath (IndexedPath p) (IndexedPath p) 

instance Typeable a => IsPath (IndexedRef (a :: *)) (IndexedPath ('Root ':/ a)) where
    toPath (IR i) = IRoot :// i
instance Typeable a => IsPath (Proxy a) (IndexedPath ('Root ':/ a)) where
    toPath _ = IRoot :// 0

instance Typeable a => CanAppend (IndexedPath as) (IndexedRef (a :: *)) (IndexedPath (as ':/ a)) where
    pappend p (IR i) = p :// i

instance Typeable a => CanAppend (IndexedPath as) (Proxy (a :: *)) (IndexedPath (as ':/ a)) where
    pappend p _ = p :// 0

instance CanAppend (Path a) a (Path a) where
    pappend = (:/)
