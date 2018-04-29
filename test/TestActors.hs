{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
module TestActors where

import "base" Data.Typeable ( Typeable, Proxy(..) )
import "base" Control.Applicative ( Const(..) )

import "dakka" Dakka.Actor ( PlainMessage(..), Actor(..), ActorContext(..), create, send, noop, HasStartState(start) )
import "dakka" Dakka.AnswerableMessage ( AnswerableMessage, answerableMessage, answer )
import "dakka" Dakka.Convert ( Convertible(..) )

import "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )
import "mtl" Control.Monad.State.Class ( modify, get, put )


-- | Actor with all bells and whistles.
newtype TestActor = TestActor
    { i :: Int
    } deriving (Show, Eq, Typeable)

instance Semigroup TestActor where
    (TestActor a) <> (TestActor b) = TestActor (a + b)

instance Monoid TestActor where
    mempty = TestActor 0

instance HasStartState TestActor where
    start = mempty

instance Actor TestActor where
    type Message TestActor = PlainMessage String
    type Creates TestActor = '[OtherActor, WithRef]
    type Capabillities TestActor = '[MonadIO]

    onSignal = noop
    onMessage (PlainMessage m) = do

        -- change interal actor state through MonadState
        modify (TestActor . succ . i)

        -- Since Capabillities contain 'MonadIO' we can use 'liftIO'.
        -- The Context has to also implement 'MonadIO' to run this behavior
        liftIO $ putStrLn m

        -- create a new Actor and send a message to it.
        -- You can only create Actors that are in 'Creates'.
        -- You can also only send messages that the actor can handle to them.
        create @OtherActor >>= (! (PlainMessage $ Msg m))

        -- Create an Actor reference from a path.
        -- The path has to be consistent.
        wr <- create @WithRef

        -- Send an AnswerableMessage to the refered actor.
        -- The message contains a reference to this actor.
        answerableMessage <$> self >>= send wr

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
    type Message Turnstile = PlainMessage TurnstileInput
    startState = Locked

    onSignal = noop
    -- Unlock on Coin. Lock un Push
    onMessage (PlainMessage m) = get >>= \case
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
    type Message OtherActor = PlainMessage Msg
    type Creates OtherActor = '[WithRef]
    onSignal = noop
    onMessage _ = do
        p <- create @WithRef
        a <- self
        p ! answerableMessage a

    startState = OtherActor


-- | Actor that handles references to other Actors
data WithRef = WithRef deriving (Show, Eq, Typeable)
instance Actor WithRef where
    type Message WithRef = AnswerableMessage (PlainMessage String)
    onSignal = noop
    onMessage a = PlainMessage "hello" `answer` a
    startState = WithRef

