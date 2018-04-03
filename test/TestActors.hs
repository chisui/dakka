{-# LANGUAGE PackageImports
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , DeriveDataTypeable
           , DataKinds
           , TypeApplications
           , TypeFamilies
           , TypeSynonymInstances
           , LambdaCase
#-}
module TestActors where

import "base" Data.Typeable
import "base" Control.Applicative ( Const(..) )

import "dakka" Dakka.Actor
import "dakka" Dakka.AnswerableMessage
import "dakka" Dakka.Convert
import "dakka" Dakka.Type.Path

import "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )
import "mtl" Control.Monad.State.Class ( modify, get, put )


-- | Actor with all bells and whistles.
newtype TestActor = TestActor
    { i :: Int
    } deriving (Show, Eq, Typeable)

instance Semigroup TestActor where
    (TestActor i) <> (TestActor j) = TestActor (i + j)

instance Monoid TestActor where
    mempty = TestActor 0

instance Actor TestActor where
    type Message TestActor = Const String
    type Creates TestActor = '[OtherActor]
    type Capabillities TestActor = '[MonadIO]
   
    onSignal = noop
    onMessage (Const m) = do

        -- change interal actor state through MonadState
        modify (TestActor . succ . i)

        -- Since Capabillities contain 'MonadIO' we can use 'liftIO'.
        -- The Context has to also implement 'MonadIO' to run this behavior
        liftIO $ putStrLn m

        -- create a new Actor and send a message to it.
        -- You can only create Actors that are in 'Creates'.
        -- You can also only send messages that the actor can handle to them.
        create @OtherActor >>= (! (Const $ Msg m))

        -- Create an Actor reference from a path.
        -- The path has to be consistent.
        wr <- self <$/> Proxy @OtherActor <$/> Proxy @WithRef

        -- Send an AnswerableMessage to the refered actor.
        -- The message contains a reference to this actor.
        AnswerableMessage <$> self >>= (send wr)

apply :: forall proxy a b. (Convertible a b, Show b) => proxy b -> a -> IO ()
apply _ = print . (convert :: a -> b)
t :: Const String Int -> IO ()
t = apply (Proxy @(Const String Int))

-- | Simple Finite state machine example.
-- Shamelessly ripped from https://en.wikipedia.org/wiki/Finite-state_machine#Example:_coin-operated_turnstile

-- | Turnstile state
data Turnstile
    = Locked
    | Unlocked
  deriving (Show, Eq, Typeable)

-- | Turnstile message
data TurnstileInput
    = Coin
    | Push
  deriving (Show, Eq, Typeable)

instance Actor Turnstile where
    type Message Turnstile = Const TurnstileInput
    startState = Locked

    onSignal = noop
    -- Unlock on Coin. Lock un Push
    onMessage (Const m) = get >>= \case
        Locked -> case m of
                    Coin -> put Unlocked
                    Push -> return ()
        Unlocked -> case m of
                        Coin -> return ()
                        Push -> put Locked


-- | Actor with custom message type.
-- This one also communicates with another actor and expects a response.
newtype Msg = Msg String deriving (Show, Eq, Typeable)
instance Convertible String Msg where
    convert = Msg

data OtherActor = OtherActor deriving (Show, Eq, Typeable)

instance Actor OtherActor where
    type Message OtherActor = Const Msg
    type Creates OtherActor = '[WithRef]
    onSignal = noop
    onMessage m = do
        p <- create @WithRef
        a <- self
        p ! AnswerableMessage a

    startState = OtherActor


-- | Actor that handles references to other Actors
data WithRef = WithRef deriving (Show, Eq, Typeable)
instance Actor WithRef where
    type Message WithRef = AnswerableMessage (Const String)
    onSignal = noop
    onMessage a = Const "hello" `answer` a
    startState = WithRef

