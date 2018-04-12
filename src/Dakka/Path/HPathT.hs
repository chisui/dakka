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
module Dakka.Path.HPathT where

import "base" Data.Functor.Classes ( liftShowsPrec )
import "base" Data.Kind ( Constraint )
import "base" Control.Applicative ( Const(..) )
import "base" Data.Typeable ( Typeable, typeOf, TypeRep )

import Dakka.Convert

import Dakka.Path.Base


-- | A typesafe path with indices.
data HPathT (p :: Path *) b where
    -- | the root of a path.
    IRoot :: b -> HPathT ('Root a) b
    -- | a sub path.
    -- Since 'a' has to be 'Typeable' we don't need to story anything 
    -- and 'a' can be a phantom type variable.
    (://) :: HPathT as b -> b -> HPathT (as ':/ a) b
infixl 5 ://

deriving instance Functor  (HPathT p)
deriving instance Typeable (HPathT p b)

instance (p `AllSegmentsImplement` Typeable) 
         => Convertible (HPathT p a) (Path (TypeRep, a)) where
    convert = demotePath

instance (Show a, p `AllSegmentsImplement` Typeable)
         => Show (HPathT p a) where
    showsPrec i = liftShowsPrec showDemoted undefined i . demotePath
      where
        showDemoted _ (t, n) s = show t ++ ':' : show n ++ s

type family AllSegmentsImplement (p :: Path k) (c :: k -> Constraint) 
    :: Constraint where
    AllSegmentsImplement ('Root a)  c = (c a)
    AllSegmentsImplement (as ':/ a) c = (c a, AllSegmentsImplement as c)

root' :: b -> HPathT ('Root a) b
root' = IRoot

root :: forall a b. Monoid b => HPathT ('Root a) b
root = root' mempty

-- | Retrieve the type of the tip of a path.
typeOfTip :: forall p a. Typeable (Tip p) => HPathT p a -> TypeRep
typeOfTip _ = typeOf @(Tip p) undefined

-- | Retrieve the demoted tip.
demoteTip :: Typeable (Tip p) => HPathT p a -> (TypeRep, a)
demoteTip p@(_ :// a) = (typeOfTip p, a)
demoteTip p@(IRoot a) = (typeOfTip p, a)

-- | Demote a path.
demotePath :: (p `AllSegmentsImplement` Typeable) => HPathT p a -> Path (TypeRep, a)
demotePath p@(IRoot _)  = Root $ demoteTip p
demotePath p@(p' :// _) = demotePath p' :/ demoteTip p

ref :: forall b a. a -> Const a b
ref = Const

