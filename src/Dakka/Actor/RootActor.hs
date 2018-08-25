{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
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

import           "base" GHC.Generics    (Generic)
import           "base" Type.Reflection (Typeable)

import           "binary" Data.Binary   (Binary)


import           Dakka.Actor.Base       (Actor, ActorContext, Creates,
                                         Signal (Created), behavior, create)
import           Dakka.Constraints      (ImplementedByAll (..), type (∈), noop)
import           Dakka.Types            (showsType)


data RootActor (l :: [*]) = RootActor
    deriving (Eq, Ord, Generic, Binary)

instance Semigroup (RootActor l) where
    (<>) = const id
instance Monoid (RootActor l) where
    mempty = RootActor

instance Typeable l => Show (RootActor l) where
    showsPrec d _
        = showParen (d > 10)
        $ showString "RootActor "
        . showsType @l


instance ( Actor `ImplementedByAll` Creates (RootActor l)
         , CanCreateAll l (RootActor l)
         , Typeable l
         ) => Actor (RootActor (l :: [*])) where
    type Creates (RootActor l) = l
    behavior = \case
        (Left Created) -> createAllOnce @l
        _              -> noop


class Typeable l => CanCreateAll (l :: [*]) a where
    createAllOnce :: ActorContext a m => m ()

instance CanCreateAll '[] a where
    createAllOnce = noop

instance ( Actor e
         , e ∈ Creates a
         , CanCreateAll es a
         ) => CanCreateAll (e ': es) a where
    createAllOnce = create @e *> createAllOnce @es

