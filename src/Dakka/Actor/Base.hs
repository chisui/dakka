{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
module Dakka.Actor.Base
    ( HasAllCapabillities
    , CanRunAll
    , ActorContext(..)
    , create
    , Signal(..)
    , Actor(..)
    ) where

import           "base" Data.Kind                (Constraint)
import           "base" Data.Proxy               (Proxy (..))
import           "base" Data.Void                (Void)

import           "mtl" Control.Monad.State.Class (MonadState)

import           Dakka.Actor.ActorRef            (ActorRef (..))
import           Dakka.Constraints               (ImplementedByAll,
                                                  ImplementsAll, Noop (..),
                                                  RichData, type (∈))
import           Dakka.Types                     ((=~=))


-- -------------------------- --
--  ActorContext constraints  --
-- -------------------------- --

class    (m `ImplementsAll` Capabillities a, HasAllCapabillities' m (Creates a)) => HasAllCapabillities m a
instance (m `ImplementsAll` Capabillities a, HasAllCapabillities' m (Creates a)) => HasAllCapabillities m a

class    HasAllCapabillities' m cs
instance HasAllCapabillities' m '[]
instance (HasAllCapabillities m a, HasAllCapabillities' m as) => HasAllCapabillities' m (a ': as)

class    (m `CanRunAll'` Creates a, m `HasAllCapabillities` a) => CanRunAll m a
instance (m `CanRunAll'` Creates a, m `HasAllCapabillities` a) => CanRunAll m a

class    CanRunAll' m l
instance (CanRunAll' m as, CanRunAll m a) => CanRunAll' m (a ': as)
instance CanRunAll' m '[]


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
class ( m `CanRunAll` a
      , MonadState a m
      ) => ActorContext (a :: *) (m :: * -> *) | m -> a
    where
      {-# MINIMAL self, create', (send | (!)) #-}
      -- | reference to the currently running 'Actor'
      self :: m (ActorRef a)

      -- | Creates a new `Actor` of type 'b' with provided start state
      create' :: (Actor b, b ∈ Creates a) => proxy b -> m (ActorRef b)

      -- | Send a message to another actor
      send :: Actor b => ActorRef b -> Message b -> m ()
      send = (!)

      -- | Alias for 'send' to enable akka style inline send.
      (!) :: Actor b => ActorRef b -> Message b -> m ()
      (!) = send


create :: forall b a m.
  ( ActorContext a m
  , Actor b
  , b ∈ Creates a
  ) => m (ActorRef b)
create = create' Proxy


-- ------- --
--  Actor  --
-- ------- --

data Signal where
    Created :: Signal
    Obit    :: Actor a => ActorRef a -> a -> Signal

deriving instance Show Signal
instance Eq Signal where
    Created    == Created    = True
    Obit r0 a0 == Obit r1 a1 = r0 =~= r1 && a0 =~= a1
    _          == _          = False


type Msg a = Either Signal (Message a)

class ( RichData a
      , RichData (Message a)
      , Actor `ImplementedByAll` Creates a
      ) => Actor (a :: *)
    where

      -- | List of all types of actors that this actor may create in its lifetime.
      type Creates a :: [*]
      type Creates a = '[]

      -- | Type of Message this Actor may recieve
      type Message a :: *
      type Message a = Void

      -- | List of all additional Capabillities the ActorContext has to provide For this Actors Behavior.
      type Capabillities a :: [(* -> *) -> Constraint]
      type Capabillities a = '[]

      -- | The behavior of this Actor
      behavior :: forall m.
                ( m `CanRunAll` a
                , ActorContext a m
                , m `HasAllCapabillities` a
                ) => Msg a -> m ()
      behavior _ = noop

      startState :: a
      default startState :: Monoid a => a
      startState = mempty

