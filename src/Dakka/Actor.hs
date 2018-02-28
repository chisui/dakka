{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , DataKinds
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , StandaloneDeriving
           , DeriveDataTypeable
           , TupleSections
           , PackageImports
           , TypeOperators
           , ConstraintKinds
           , PolyKinds
           , RankNTypes
           , UndecidableInstances
           , UndecidableSuperClasses
#-}
module Dakka.Actor where

import Dakka.Constraints

import "base" Data.Kind ( Constraint )
import "base" Type.Reflection ( Typeable )
import "base" Data.Proxy ( Proxy(..) )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.State.Class ( MonadState, modify )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )


-- | A path of an Actor inside the Actor system.
-- FIXME This is a Bullshit implementation
data Path a = Path {-# UNPACK #-} !Word
    deriving (Show, Eq, Typeable)

-- | Execution Context of an 'Actor'.
-- Has to provide ways to:
--
--     * change the state of an `Actor` (through `MonadState`)
--
--     * send messages to other actors
--
--     * create new actors.
-- 
class (Actor a, MonadState a m) => ActorContext (a :: *) (m :: * -> *) | m -> a where
    {-# MINIMAL create, (send | (!)) #-}

    -- | Creates a new `Actor` of type 'b' with provided start state
    create :: (Actor b, b :∈ Creates a) => b -> m (Path b)

    -- | If the Actors state is a 'Monoid' no inital State has to be provided.
    create' :: (Actor b, b :∈ Creates a, Monoid b) => m (Path b)
    create' = create mempty

    -- | Send a message to another actor
    send :: Actor b => Path b -> Message b -> m ()
    send = (!)

    -- | Alias for 'send' to enable akka style inline send.
    (!) :: Actor b => Path b -> Message b -> m ()
    (!) = send

-- ---------------- --
-- MockActorContext --
-- ---------------- --

-- | Encapsulates a Message sent to an actor
data Envelope = forall a. Actor a => Envelope
    { to  :: Path a
    , msg :: Message a
    }
deriving instance Show Envelope


-- | Encapsulates the intent to create another actor.
data CreationIntent= forall a. Actor a => CreationIntent a
deriving instance Show CreationIntent

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
--
newtype MockActorContext a v = MockActorContext
    (StateT a (Writer [Either Envelope CreationIntent]) v)
  deriving (Functor, Applicative, Monad, MonadState a, MonadWriter [Either Envelope CreationIntent])

instance Actor a => ActorContext a (MockActorContext a) where

    create a = do
      tell [Right $ CreationIntent a]
      return $ Path 0
    
    p ! m = tell [Left $ Envelope p m]

execMock :: MockActorContext a b -> a -> (a, [Either Envelope CreationIntent])
execMock (MockActorContext ctx) = runWriter . execStateT ctx


type Behavior a = forall m. ActorContext a m => Message a -> m ()
type RichData a = (Show a, Eq a, Typeable a)
class (RichData a, RichData (Message a), Actor `ImplementedByAll` Creates a) => Actor (a :: *) where
    type Creates a :: [*]
    type Creates a = '[]
  
    type Message a :: *
    behavior :: Behavior a

behaviorOf :: Proxy a -> Behavior a 
behaviorOf = const behavior

-- Test --

newtype TestActor = TestActor
    { i :: Int
    } deriving (Show, Eq, Typeable)
instance Actor TestActor where
  type Message TestActor = String
  type Creates TestActor = '[OtherActor]
  behavior m = do
      modify (TestActor . succ . i) 
      p <- create OtherActor
      p ! Msg m

newtype Msg = Msg String deriving (Show, Eq, Typeable)
data OtherActor = OtherActor deriving (Show, Eq, Typeable)
instance Actor OtherActor where
  type Message OtherActor = Msg
  behavior m = undefined
