{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Dakka.Actor.RootActor where

import "base" Data.Typeable ( Typeable, typeRep )
import "base" GHC.Generics ( Generic )

import Dakka.Actor.Base ( Actor( Creates, behavior ), Signal(..), ActorAction, create, noop )
import Dakka.Constraints ( (:⊆), ImplementedByAll )
import Dakka.HasStartState ( HasStartState )


data RootActor (l :: [*]) = RootActor
    deriving (Eq, Ord, Typeable, Generic)
instance Semigroup (RootActor l) where
    (<>) = const id
instance Monoid (RootActor l) where
    mempty = RootActor
instance HasStartState (RootActor l)

instance Typeable l => Show (RootActor l) where
    showsPrec d r = showParen (d > 10) $ showString "RootActor " . showsPrec 10 (typeRep r)

instance (IsRootActor l, Typeable l, l :⊆ l) => Actor (RootActor (l :: [*])) where
    type Creates (RootActor l) = l
    behavior (Left Created) = initRootActor (RootActor @l)
    behavior _              = pure ()

class (Actor `ImplementedByAll` l, Typeable l) => IsRootActor (l :: [*]) where
    initRootActor :: (Actor a, l :⊆ Creates a) => RootActor l -> ActorAction m a r

instance IsRootActor '[] where
    initRootActor = noop

instance (Actor a, IsRootActor as) => IsRootActor (a ': as) where
    initRootActor _ = do
        _ <- create @a
        initRootActor (RootActor @as)

