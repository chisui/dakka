{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , TypeApplications
           , DefaultSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
           , DataKinds
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , DeriveDataTypeable
           , PackageImports
           , TypeOperators
           , ConstraintKinds
           , PolyKinds
           , RankNTypes
           , UndecidableInstances
           , UndecidableSuperClasses
#-}
module Dakka.Actor where

import "base" Data.Kind ( Constraint )
import "base" Data.Typeable ( Typeable, cast, typeRep )
import "base" Data.Proxy ( Proxy(..) )
import "containers" Data.Tree ( Tree(..) )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )
import "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState, modify )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Constraints
import Dakka.Type.Path
import Dakka.Type.Tree
import Dakka.Convert


type ActorRef p = IndexedPath p

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
      , a ~ Select p t
      ) => ActorContext 
           (t :: Tree *)
           (p :: Path *)
           (a :: *)
           (m :: * -> *)
      | m -> a, m -> p, m -> t where
      {-# MINIMAL self, create', (send | (!)) #-}

      -- | reference to the currently running 'Actor'
      self :: m (ActorRef p)

      -- | Creates a new `Actor` of type 'b' with provided start state
      create' :: (Actor b, b :∈ Creates a) => Proxy b -> m (ActorRef (p ':/ b))

      -- | Send a message to another actor
      send :: Actor (Tip b) => ActorRef b -> Message (Tip b) -> m ()
      send = (!)

      -- | Alias for 'send' to enable akka style inline send.
      (!) :: Actor (Tip b) => ActorRef b -> Message (Tip b) -> m ()
      (!) = send

create :: (Actor b, Actor a, ActorContext t p a m, b :∈ Creates a) => m (ActorRef (p ':/ b))
create = create' Proxy

-- ------- --
--  Actor  --
-- ------- --

-- | A Behavior of an 'Actor' defines how an Actor reacts to a message given a specific state.
-- A Behavior may be executed in any 'ActorContext' that has all of the actors 'Capabillities'.
type Behavior a = forall t p m. (Tip p ~ a, ActorContext t p a m, m `ImplementsAll` Capabillities a) => Message a -> m ()


class ( RichData a
      , RichData (Message a)
      , Actor `ImplementedByAll` Creates a
      ) => Actor
             (a :: *)
    where
      
      -- | List of all types of actors that this actor may create in its lifetime.
      type Creates a :: [*]
      type Creates a = '[]
  
      -- | Type of Message this Actor may recieve
      type Message a :: *

      -- | List of all additional Capabillities the ActorContext has to provide For this Actors Behavior.
      type Capabillities a :: [(* -> *) -> Constraint]
      type Capabillities a = '[]

      -- | This Actors behavior
      behavior :: Behavior a

      startState :: a
      default startState :: Monoid a => a
      startState = mempty

-- | A pure 'Actor' is one that has no additional Capabillities besides what a 
-- 'ActorContext' provides.
type PureActor a = (Actor a, Capabillities a ~ '[])

-- | A leaf 'Actor' is one that doesn't create any children.
type LeafActor a = (Actor a, Creates a ~ '[])

behaviorOf :: proxy a -> Behavior a 
behaviorOf = const behavior

-- ------------- --
--  ActorSystem  --
-- ------------- --

-- | Create an actor system tree from a list of base actors.
-- The Tree is constructed by creating 
type family ActorSystemTree (r :: [*]) :: Tree * where
    ActorSystemTree r = 'Node () (ActorSystemSubTrees r)

type family ActorSystemSubTrees (r :: [*]) :: [Tree *] where
    ActorSystemSubTrees '[]       = '[]
    ActorSystemSubTrees (a ': as) = ActorSystemSubTree a ': ActorSystemSubTrees as

type family ActorSystemSubTree (a :: *) :: Tree * where
    ActorSystemSubTree a = 'Node a (ActorSystemSubTrees (Creates a))  

