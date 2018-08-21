{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PackageImports          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE Safe                    #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Dakka.Actor.RootActor where

import           "base" Data.Proxy      (Proxy (..))
import           "base" Data.Void       (Void)
import           "base" GHC.Generics    (Generic)
import           "base" Type.Reflection

import           "binary" Data.Binary   (Binary)


import           Dakka.Actor.Base       (Actor (Creates, Message, behavior),
                                         ActorAction, Signal (..), create, noop)
import           Dakka.Constraints      ((:⊆), ImplementedByAll)
import           Dakka.HasStartState    (HasStartState)


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


