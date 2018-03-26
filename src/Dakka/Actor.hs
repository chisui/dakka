{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GADTs
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
           , ScopedTypeVariables
           , InstanceSigs
           , RankNTypes
           , UndecidableInstances
           , UndecidableSuperClasses
#-}
module Dakka.Actor where

import "base" Data.Kind ( Constraint )
import "base" Data.Typeable ( Typeable, TypeRep, cast, typeRep )
import "base" Data.Proxy ( Proxy(..) )
import "containers" Data.Tree ( Tree(..) )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )
import "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState, modify )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Constraints
import Dakka.Type.Path ( Path(..), Tip, IndexedRef(..) )
import Dakka.Type.Tree
import Dakka.Convert

-- ---------- --
--  ActorRef  --
-- ---------- --

data ActorRef (p :: Path *) where
    ARoot :: Actor a => ActorRef ('Root a)
    (://) :: (a :∈ Creates (Tip as), Actor a) => ActorRef as -> Word -> ActorRef (as ':/ a)

deriving instance Show (ActorRef p)
deriving instance Eq (ActorRef p)
deriving instance Typeable (ActorRef p)

instance Convertible (ActorRef p) (Path (Word, TypeRep)) where
    convert :: forall p. (ActorRef p) -> (Path (Word, TypeRep)) 
    convert ARoot = Root (0, typeRep (Proxy @(Tip p)))
    convert (as :// a) = convert as :/ (a, typeRep (Proxy @(Tip p)))

type family ConsistentActorPath (p :: Path *) :: Constraint where
    ConsistentActorPath ('Root a)  = (Actor a)
    ConsistentActorPath (as ':/ a) = (a :∈ Creates (Tip as), ConsistentActorPath as)

(<$/>) :: ( Functor f
          , Actor a
          , ConsistentActorPath (as ':/ a)
          , PathSegment seg
          ) => f (ActorRef as) -> seg a -> f (ActorRef (as ':/ a))
f <$/> a = (</> a) <$> f
         
class PathSegment seg where
    (</>) :: ( ConsistentActorPath (as ':/ a)
             , Actor a
             ) => ActorRef as -> seg a -> ActorRef (as ':/ a)

instance PathSegment Proxy where
    ref </> _ = ref </> IR 0
instance PathSegment IndexedRef where
    ref </> (IR i) = ref :// i

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
class ( ConsistentActorPath p
      , MonadState (Tip p) m
      ) => ActorContext 
           (p :: Path *)
           (m :: * -> *)
      | m -> p
    where
      {-# MINIMAL self, create', (send | (!)) #-}

      -- | reference to the currently running 'Actor'
      self :: m (ActorRef p)

      -- | Creates a new `Actor` of type 'b' with provided start state
      create' :: (Actor b, b :∈ Creates (Tip p), ConsistentActorPath (p ':/ b)) => Proxy b -> m (ActorRef (p ':/ b))

      -- | Send a message to another actor
      send :: Actor b => ActorRef (bs ':/ b) -> Message b -> m ()
      send = (!)

      -- | Alias for 'send' to enable akka style inline send.
      (!) :: Actor b => ActorRef (bs ':/ b) -> Message b -> m () 
      (!) = send

create :: (Actor b, ActorContext p m, b :∈ Creates (Tip p), ConsistentActorPath (p ':/ b)) => m (ActorRef (p ':/ b))
create = create' Proxy

-- ------- --
--  Actor  --
-- ------- --

-- | A Behavior of an 'Actor' defines how an Actor reacts to a message given a specific state.
-- A Behavior may be executed in any 'ActorContext' that has all of the actors 'Capabillities'.
type Behavior a = forall p m. (Actor a, Tip p ~ a, ActorContext p m, m `ImplementsAll` Capabillities a) => Message a -> m ()


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

