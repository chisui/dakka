{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
module Dakka.Path.IndexedPath where

import "base" Data.Functor.Classes ( liftShowsPrec )
import "base" Data.Kind ( Constraint )
import "base" Data.Typeable ( Typeable, typeOf, TypeRep )

import Dakka.Convert

import Dakka.Path.Base


root :: IndexedPath ('Root a)
root = IRoot

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
demoteTip p@IRoot     = (typeOfTip p, 0)

-- | Demote a path.
demotePath :: p `AllSegmentsImplement` Typeable => IndexedPath p -> Path (TypeRep, Word)
demotePath p@IRoot = Root $ demoteTip p
demotePath p@(p' :// _) = demotePath p' :/ demoteTip p

instance (p `AllSegmentsImplement` Typeable) => Convertible (IndexedPath p) (Path (TypeRep, Word)) where
    convert = demotePath

instance (p `AllSegmentsImplement` Typeable) => Show (IndexedPath p) where
    showsPrec i = liftShowsPrec showDemoted undefined i . demotePath
      where
        showDemoted _ (t, 0) s = show t ++ s
        showDemoted _ (t, n) s = show t ++ ':' : show n ++ s

-- | Represents a Segement of an 'IndexedPath'.
newtype IndexedRef a
    = IR Word
  deriving (Eq, Ord, Functor)

instance Enum (IndexedRef a) where
  fromEnum (IR e) = fromEnum e
  toEnum = IR . toEnum

ref :: Typeable a => Word -> IndexedRef a
ref = IR

