{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Dakka.Actor.RootActor where

import "base" GHC.Generics ( Generic )
import "base" Type.Reflection 
import "base" Data.Proxy ( Proxy(..) ) 
import "base" Data.Void ( Void ) 

import "binary" Data.Binary ( Binary )


import Dakka.Actor.Base ( Actor( Creates, Message, behavior )
                        , Signal(..), ActorAction
                        , create, noop
                        )
import Dakka.Constraints ( (:⊆), ImplementedByAll )
import Dakka.HasStartState ( HasStartState )


data RootActor (l :: [*]) = RootActor
    deriving (Eq, Ord, Generic, Binary)
instance Semigroup (RootActor l) where
    (<>) = const id
instance Monoid (RootActor l) where
    mempty = RootActor
instance HasStartState (RootActor l)

instance IsRootActor l => Show (RootActor l) where
    showsPrec d r = showParen (d > 10)
                  $ showString "RootActor <<"
                  . shows (createsActors r)
                  . showString ">>"

instance (IsRootActor l, Typeable l, l :⊆ l) => Actor (RootActor (l :: [*])) where
    type Creates (RootActor l) = l
    type Message (RootActor l) = Void
    behavior (Left Created) = initRootActor (RootActor @l)
    behavior _              = pure ()

class (Actor `ImplementedByAll` l, Typeable l) => IsRootActor (l :: [*]) where
    initRootActor :: (Actor a, l :⊆ Creates a) => RootActor l -> ActorAction m a
    createsActors :: RootActor l -> [SomeTypeRep]

instance IsRootActor '[] where
    initRootActor   = noop
    createsActors _ = []

instance (Actor a, IsRootActor as) => IsRootActor (a ': as) where
    initRootActor _ = do
        _ <- create @a
        initRootActor (RootActor @as)
    createsActors _ = someTypeRep (Proxy @a) : createsActors (RootActor @as)


