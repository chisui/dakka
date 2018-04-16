{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    ( HPathT(..)
    , root
    , ref
    , ActorRef(..)
    , ActorRefConstraints
    , ActorContext(..)
    , create
    , ActorMessage(..)
    , PlainMessage(..)
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
import "base" GHC.Generics ( Generic ) 
import "base" Data.Proxy ( Proxy(..) )
import "base" Text.ParserCombinators.ReadP ( readP_to_S, readS_to_P, string )
import "base" Control.Applicative ( (*>) )

import "mtl" Control.Monad.State.Class ( MonadState )

import Dakka.Constraints
    ( (:∈)
    , ImplementsAll, ImplementedByAll
    , RichData
    , (=~=)
    )
import Dakka.Path
    ( Path(..), Tip, PRoot, root
    , ref, HPathT(..), AllSegmentsImplement
    )

import Dakka.Convert ( Convertible(..) )
import Dakka.HasStartState ( HasStartState(..) )


-- ---------- --
--  ActorRef  --
-- ---------- --

type ActorRefConstraints p
  = ( ConsistentActorPath p
    , Typeable p
    , Actor (Tip p)
    , Actor (PRoot p)
    , p `AllSegmentsImplement` Actor
    , p `AllSegmentsImplement` Typeable
    )

type family ConsistentActorPath (p :: Path *) :: Constraint where
    ConsistentActorPath ('Root a)  = (Actor a)
    ConsistentActorPath (as ':/ a) = (a :∈ Creates (Tip as), ConsistentActorPath as)

class Typeable ref => ActorRef (ref :: Path * -> *) where
    eqRef :: (ActorRefConstraints p, ActorRefConstraints q) => ref p -> ref q -> Bool
    default eqRef :: ( Eq (ref p)
                     , ActorRefConstraints p
                     , ActorRefConstraints q
                     , Typeable (ref p)
                     , Typeable (ref q)
                     ) => ref p -> ref q -> Bool
    eqRef = (=~=)

    showsRef :: ActorRefConstraints p => Int -> ref p -> ShowS
    default showsRef :: (Show (ref p), ActorRefConstraints p) => Int -> ref p -> ShowS
    showsRef = showsPrec

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
class ( ActorRefConstraints p 
      , MonadState (Tip p) m
      , ActorRef (Ref m)
      ) => ActorContext 
          (p :: Path *)
          (m :: * -> *)
      | m -> p
    where
      {-# MINIMAL self, create', (send | (!)) #-}
      type Ref m :: Path * -> *

      -- | reference to the currently running 'Actor'
      self :: ConsistentActorPath p => m (Ref m p)

      -- | Creates a new `Actor` of type 'b' with provided start state
      create' :: ( Actor b
                 , b :∈ Creates (Tip p)
                 , ActorRefConstraints (p ':/ b)
                 ) => Proxy b -> m (Ref m (p ':/ b))

      -- | Send a message to another actor
      send :: ( PRoot p ~ PRoot b
              , Actor (Tip b)
              , ActorRefConstraints b
              ) => Ref m b -> Message (Tip b) (Ref m) b -> m ()
      send = (!)

      -- | Alias for 'send' to enable akka style inline send.
      (!) :: ( PRoot p ~ PRoot b
             , Actor (Tip b)
             , ActorRefConstraints b
             ) => Ref m b -> Message (Tip b) (Ref m) b -> m () 
      (!) = send

create :: ( Actor b
          , ActorContext p m 
          , b :∈ Creates (Tip p)
          , ConsistentActorPath (p ':/ b)
          ) => m (Ref m (p ':/ b))
create = create' Proxy

-- ------- --
--  Actor  --
-- ------- --

-- | A Behavior of an 'Actor' defines how an Actor reacts to a message given a specific state.
-- A Behavior may be executed in any 'ActorContext' that has all of the actors 'Capabillities'.
type ActorAction (m :: * -> *) (a :: *) (p :: Path *)
  = ( Actor a
    , Tip p ~ a
    , ActorContext p m
    , m `ImplementsAll` Capabillities a
    ) => m ()

type Behavior a = forall (m :: * -> *) (p :: Path *). Either (Signal m a) (Message a (Ref m) p) -> ActorAction m a p

data Signal (m :: * -> *) (a :: *) where
    Created :: Actor a => Signal m a
    Obit    :: ( Actor a
               , Typeable (p ':/ c)
               , Typeable (Ref m)
               , Eq (Ref m (p ':/ c))
               , Show (Ref m (p ':/ c))
               , Tip p ~ a
               , ActorRefConstraints (p ':/ c)
               ) => Ref m (p ':/ c) -> Signal m a

deriving instance (Typeable m, Typeable a) => Typeable (Signal m a)

instance Eq (Signal m a) where
    Created  == Created  = True
    (Obit a) == (Obit b) = a =~= b
    _        == _        = False

instance Show (Signal m a) where
    showsPrec d s = showParen (d > 10) $
        case s of
            Created  -> showString "Created <<"
                      . showsPrec 11 (typeRep s)
                      . showString ">>"
            (Obit r) -> showString "Obit "
                      . showsPrec 11 r

newtype PlainMessage m r p = PlainMessage
    { getPlainMessage :: m }
  deriving (Eq, Ord, Typeable, Generic)

instance Show m => Show (PlainMessage m r p) where
    showsPrec d (PlainMessage m) = showParen (d > 10)
                                 $ showString "PlainMessage "
                                 . showsPrec 11 m

instance Read m => Read (PlainMessage m r p) where
    readsPrec d = readParen (d > 10)
                $ readP_to_S
                $ string "PlainMessage " *> fmap PlainMessage (readS_to_P reads)

instance (Typeable m, Eq m, Show m) => ActorMessage (PlainMessage m)

instance Convertible a b => Convertible (PlainMessage a r p) (PlainMessage b x y) where
    convert (PlainMessage m) = PlainMessage $ convert m

class Typeable m => ActorMessage (m :: (Path * -> *) -> Path * -> *) where
    eqMsg :: ActorRef r => m r p -> m r p -> Bool
    default eqMsg :: Eq (m r p) => m r p -> m r p -> Bool
    eqMsg = (==)
    
    showsMsg :: ActorRef r => Int -> m r p -> ShowS
    default showsMsg :: Show (m r p) => Int -> m r p -> ShowS
    showsMsg = showsPrec

class ( RichData a
      , ActorMessage (Message a)
      , Actor `ImplementedByAll` Creates a
      ) => Actor (a :: *)
    where
      {-# MINIMAL behavior | (onMessage, onSignal) #-}

      -- | List of all types of actors that this actor may create in its lifetime.
      type Creates a :: [*]
      type Creates a = '[]
  
      -- | Type of Message this Actor may recieve
      type Message a :: (Path * -> *) -> Path * -> *
      type Message a = PlainMessage ()

      -- | List of all additional Capabillities the ActorContext has to provide For this Actors Behavior.
      type Capabillities a :: [(* -> *) -> Constraint]
      type Capabillities a = '[]

      -- | What this Actor does when recieving a message
      onMessage :: Message a (Ref m) p -> ActorAction m a p
      onMessage = behavior . Right

      -- | What this Actor does when recieving a Signal
      onSignal :: Signal m a -> ActorAction m a p 
      onSignal = behavior . Left

      -- | The behavior of this Actor
      behavior :: Either (Signal m a) (Message a (Ref m) p) -> ActorAction m a p
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

