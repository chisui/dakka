{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Dakka.Actor.Base
    ( ActorRef(..)
    , ActorContext(..)
    , create
    , Actor(..)
    , ActorAction
    , Behavior
    , behaviorOf
    , LeafActor
    , PureActor
    , Signal(..)
    , noop
    ) where

import "base" Data.Kind ( Constraint )
import "base" Data.Typeable ( Typeable, typeRep )
import "base" Data.Proxy ( Proxy(..) )
import "base" GHC.Generics ( Generic )

import "bytestring" Data.ByteString ( ByteString )

import "mtl" Control.Monad.State.Class ( MonadState )

import Dakka.Constraints
    ( (:∈)
    , ImplementsAll, ImplementedByAll
    , RichData
    , (=~=)
    )

import Dakka.HasStartState ( HasStartState(..) )


-- ---------- --
--  ActorRef  --
-- ---------- --

newtype ActorRef a = ActorRef { actorId :: ByteString }
  deriving (Eq, Generic, Functor)

eqRef :: (Typeable a, Typeable b) => ActorRef a -> ActorRef b -> Bool
eqRef refA@(ActorRef idA) refB@(ActorRef idB) = idA == idB && typeRep refA == typeRep refB

instance Typeable a => Show (ActorRef a) where
    showsPrec d ref@(ActorRef aId) = showParen (d > 10)
                                   $ showString "ActorRef <<"
                                   . shows (typeRep ref)
                                   . showString ">>@"
                                   . shows aId

-- -------------- --
--  ActorContext  --
-- -------------- --

-- | Execution Context of an 'Actor'.
-- Has to provide ways to:
--
--     * change the state of an `Actor` (through `MonadState`)
--
--     * send messages to other actors
--
--     * create new actors.
-- 
class ( Actor a
      , MonadState a m
      ) => ActorContext a (m :: * -> *) | m -> a
    where
      {-# MINIMAL self, create', (send | (!)) #-}
      -- | reference to the currently running 'Actor'
      self :: m (ActorRef a)

      -- | Creates a new `Actor` of type 'b' with provided start state
      create' :: ( Actor a 
                 , Actor b
                 , b :∈ Creates a 
                 ) => Proxy b -> m (ActorRef b) 

      -- | Send a message to another actor
      send :: Actor b => ActorRef b -> Message b -> m ()
      send = (!)

      -- | Alias for 'send' to enable akka style inline send.
      (!) :: Actor b => ActorRef b -> Message b -> m ()
      (!) = send


create :: forall b a m.
  ( Actor b
  , Actor a
  , ActorContext a m
  , b :∈ Creates a 
  ) => m (ActorRef b)
create = create' Proxy

-- ------- --
--  Actor  --
-- ------- --

-- | A Behavior of an 'Actor' defines how an Actor reacts to a message given a specific state.
-- A Behavior may be executed in any 'ActorContext' that has all of the actors 'Capabillities'.
type ActorAction (m :: * -> *) (a :: *)
  = ( Actor a
    , ActorContext a m
    , m `ImplementsAll` Capabillities a
    ) => m ()

type Behavior a = forall (m :: * -> *). Either (Signal m a) (Message a) -> ActorAction m a

data Signal (m :: * -> *) (a :: *) where
    Created :: Actor a => Signal m a
    Obit    :: Actor a => ActorRef a -> a -> Signal m a

deriving instance (Typeable m, Typeable a) => Typeable (Signal m a)

instance ActorContext a m => Eq (Signal m a) where
    Created       == Created       = True
    (Obit refa a) == (Obit refb b) = refa `eqRef` refb && a =~= b
    _             == _             = False

instance ActorContext a m => Show (Signal m a) where
    showsPrec d s = showParen (d > 10) $
        case s of
            Created    -> showString "Created <<"
                        . showsPrec 11 (typeRep s)
                        . showString ">>"
            (Obit r a) -> showString "Obit "
                        . shows r
                        . showString " "
                        . shows a

class ( RichData a
      , RichData (Message a)
      , Actor `ImplementedByAll` Creates a
      ) => Actor (a :: *)
    where
      {-# MINIMAL behavior | (onMessage, onSignal) #-}

      -- | List of all types of actors that this actor may create in its lifetime.
      type Creates a :: [*]
      type Creates a = '[]
  
      -- | Type of Message this Actor may recieve
      type Message a :: *
      type Message a = ()

      -- | List of all additional Capabillities the ActorContext has to provide For this Actors Behavior.
      type Capabillities a :: [(* -> *) -> Constraint]
      type Capabillities a = '[]

      -- | What this Actor does when recieving a message
      onMessage :: Message a -> ActorAction m a
      onMessage = behavior . Right

      -- | What this Actor does when recieving a Signal
      onSignal :: Signal m a -> ActorAction m a
      onSignal = behavior . Left

      -- | The behavior of this Actor
      behavior :: Either (Signal m a) (Message a) -> ActorAction m a
      behavior = either onSignal onMessage

      startState :: a
      default startState :: HasStartState a => a
      startState = start 

-- | A pure 'Actor' is one that has no additional Capabillities besides what a 
-- 'ActorContext' provides.
type PureActor a = (Actor a, Capabillities a ~ '[])

-- | A leaf 'Actor' is one that doesn't create any children.
type LeafActor a = (Actor a, Creates a ~ '[])

behaviorOf :: proxy a -> Behavior a
behaviorOf = const behavior

noop :: (Applicative f, Applicative g) => f (g ())
noop = pure noop'

noop' :: Applicative f => f ()
noop' = pure ()

