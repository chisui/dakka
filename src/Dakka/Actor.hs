{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , DefaultSignatures
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , DataKinds
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , DeriveDataTypeable
           , DeriveGeneric
           , PackageImports
           , TypeOperators
           , ConstraintKinds
           , PolyKinds
           , RankNTypes
           , UndecidableInstances
           , UndecidableSuperClasses
#-}
module Dakka.Actor where

import           "base" Data.Kind ( Constraint )
import           "base" Data.Typeable ( Typeable, cast )
import           "base" GHC.Generics ( Generic, Rep, R, M1(..), K1(..), U1(..) )
import qualified "base" GHC.Generics as G
import           "base" Data.Proxy ( Proxy(..) )
import           "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )

import           "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import           "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import           "mtl" Control.Monad.State.Class ( MonadState, modify )
import           "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import           Dakka.Constraints


-- | A path of an Actor inside the Actor system.
-- FIXME This is a Bullshit implementation
newtype Path a = Path Word
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
    {-# MINIMAL self, create', (send | (!)) #-}

    -- | reference to the currently running 'Actor'
    self :: m (Path a)

    create' :: (Actor b, b :∈ Creates a) => Proxy b -> m (Path b)

    -- | Send a message to another actor
    send :: Actor b => Path b -> Message b -> m ()
    send = (!)

    -- | Alias for 'send' to enable akka style inline send.
    (!) :: Actor b => Path b -> Message b -> m ()
    (!) = send

-- | Creates a new `Actor` of type 'b' with provided start state
create :: (Actor b, Actor a, b :∈ Creates a, ActorContext a m) => m (Path b)
create = create' Proxy

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
data CreationIntent = forall a. Actor a => CreationIntent (Proxy a)
deriving instance Show CreationIntent

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext a v = MockActorContext
    (StateT a (Writer [Either Envelope CreationIntent]) v)
  deriving (Functor, Applicative, Monad, MonadState a, MonadWriter [Either Envelope CreationIntent])

instance Actor a => ActorContext a (MockActorContext a) where

    self = return $ Path 0

    create' proxy = do
        tell [Right $ CreationIntent proxy]
        return $ Path 0

    p ! m = tell [Left $ Envelope p m]

class DefaultValue a where
    defaultValue :: a
    default defaultValue :: (Generic a, GDefaultValue (Rep a)) => a
    defaultValue = G.to gDefaultvalue
class GDefaultValue (a :: * -> *) where
    gDefaultvalue :: a x
instance GDefaultValue a => GDefaultValue (M1 i c a) where
    gDefaultvalue = M1 gDefaultvalue
instance GDefaultValue U1 where
    gDefaultvalue = U1
instance DefaultValue t => GDefaultValue (K1 i t) where
    gDefaultvalue = K1 defaultValue

instance {-# OVERLAPPABLE #-} Num i => DefaultValue i where
    defaultValue = 0

-- | Execute a 'Behavior' in a 'MockActorContext'.
execMock :: MockActorContext a b -> a -> (a, [Either Envelope CreationIntent])
execMock (MockActorContext ctx) = runWriter . execStateT ctx

execMock' :: DefaultValue a => MockActorContext a b -> (a, [Either Envelope CreationIntent])
execMock' ctx = execMock ctx defaultValue

printMockRun' :: (DefaultValue a, RichData a) => MockActorContext a b -> IO ()
printMockRun' ctx = printMockRun ctx defaultValue

printMockRun :: (RichData a) => MockActorContext a b -> a -> IO ()
printMockRun ctx a = do
    let (a', msgs) = execMock ctx a
    putStrLn $ "State: " ++ show a ++ if a /= a'
        then " -> " ++ show a'
        else ""
    mapM_ (putStrLn . prettyMsg) msgs
  where
    prettyMsg :: Either Envelope CreationIntent -> String
    prettyMsg (Left (Envelope to msg)) = show to ++ " ! " ++ show msg
    prettyMsg (Right (CreationIntent a)) = "create " ++ show a

-- ------- --
--  Actor  --
-- ------- --

-- | Behavior type that is independent of any 'Actor' constraint.
type Behavior m cps msg = m `ImplementsAll` cps => msg -> m ()

-- | A Behavior of an 'Actor' defines how an Actor reacts to a message given a specific state.
-- A Behavior may be executed in any 'ActorContext' that has all of the actors 'Capabillities'.
type Behavior' a = forall m. ActorContext a m => Behavior m (Capabillities a) (Message a)

-- | To be able to route values through an actor system these values have provide certain features.
type RichData a = (Show a, Eq a, Typeable a)

class (RichData a, RichData (Message a), Actor `ImplementedByAll` Creates a) => Actor (a :: *) where
    -- | List of all types of actors that this actor may create in its lifetime.
    type Creates a :: [*]
    type Creates a = '[]

    -- | Type of Message this Actor may recieve
    type Message a :: *

    -- | List of all additional Capabillities the ActorContext has to provide For this Actors Behavior.
    type Capabillities a :: [(* -> *) -> Constraint]
    type Capabillities a = '[]

    -- | This Actors behavior
    behavior :: Behavior' a

-- | A pure 'Actor' is one that has no additional Capabillities besides what a
-- 'ActorContext' provides.
type PureActor a = (Actor a, Capabillities a ~ '[])

-- | A leaf 'Actor' is one that doesn't create any children.
type LeafActor a = (Actor a, Creates a ~ '[])

behaviorOf :: Proxy a -> Behavior' a
behaviorOf = const behavior

class a `Injectable` b where
    inject :: a -> b
instance a `Injectable` a where
    inject = id
data AnswerableMsg r = forall a. (Actor a, r `Injectable` (Message a), Eq (Path a))
    => AnswerableMsg
        { sender :: Path a
        } deriving Typeable

instance Eq (AnswerableMsg r) where
    (AnswerableMsg a) == (AnswerableMsg b) = a =~= b
deriving instance Show (AnswerableMsg r)

answer :: ActorContext a m => r -> AnswerableMsg r -> m ()
answer r (AnswerableMsg sender) = sender ! inject r

ask :: (Actor b, ActorContext a m, Message b ~ AnswerableMsg r, r `Injectable` Message a) => Path b -> m ()
ask b = do
    a <- self
    b ! AnswerableMsg a

-- -------- --
--   Test   --
-- -------- --

-- | Utillity function for equality of Typeables.
-- FIXME move to differenct module.
(=~=) :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
a =~= b = Just a == cast b

-- | Actor with all bells and whistles.
newtype TestActor = TestActor
    { i :: Int
    } deriving (Show, Eq, Typeable, Generic)
instance DefaultValue TestActor
instance Actor TestActor where
  type Message TestActor = String
  type Creates TestActor = '[OtherActor]
  type Capabillities TestActor = '[MonadIO]
  behavior m = do
      modify (TestActor . succ . i)
      liftIO $ putStrLn m
      p <- create @OtherActor
      p ! Msg m

-- | Actor with custom message type.
-- This one also communicates with another actor and expects a response.
newtype Msg = Msg String deriving (Show, Eq, Typeable, Generic)
instance Injectable String Msg where
    inject = Msg
data OtherActor = OtherActor deriving (Show, Eq, Typeable, Generic)
instance DefaultValue OtherActor
instance Actor OtherActor where
  type Message OtherActor = Msg
  type Creates OtherActor = '[WithRef]
  behavior _ = create @WithRef >>= ask

data WithRef = WithRef deriving (Show, Eq, Typeable, Generic)
instance DefaultValue WithRef
instance Actor WithRef where
    type Message WithRef = AnswerableMsg String
    behavior = answer "hello"

