{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module TestActors where

import           "base" Control.Applicative      (Const (..))
import           "base" Data.Proxy               (Proxy (..))
import           "base" GHC.Generics             (Generic)

import           "dakka" Dakka.Actor             (Actor (..), ActorContext (..),
                                                  ActorRef, Signal (Created),
                                                  create, noop, send)
import           "dakka" Dakka.AnswerableMessage (AnswerableMessage, answer,
                                                  answerableMessage)
import           "dakka" Dakka.Convert           (Convertible (..))

import           "base" Control.Monad.IO.Class   (MonadIO (liftIO))
import           "mtl" Control.Monad.State.Class (get, modify, put)
import           "binary" Data.Binary            (Binary)



-- | Actor with all bells and whistles.
newtype TestActor = TestActor
    { i :: Int }
    deriving stock    (Eq, Show, Generic)
    deriving anyclass (Binary)

instance Semigroup TestActor where
    (TestActor a) <> (TestActor b) = TestActor (a + b)

instance Monoid TestActor where
    mempty = TestActor 0

instance Actor TestActor where
    type Message TestActor = String
    type Creates TestActor = '[OtherActor, WithRef]
    type Capabillities TestActor = '[MonadIO]

    behavior = \case
        (Right m) -> do

            -- change interal actor state through MonadState
            modify (TestActor . succ . i)

            -- Since Capabillities contain 'MonadIO' we can use 'liftIO'.
            -- The Context has to also implement 'MonadIO' to run this behavior
            liftIO $ putStrLn m

            -- create a new Actor and send a message to it.
            -- You can only create Actors that are in 'Creates'.
            -- You can also only send messages that the actor can handle to them.
            create @OtherActor >>= (! Msg m)

            -- Create an Actor reference from a path.
            -- The path has to be consistent.

            wr <- create @WithRef

            -- Send an AnswerableMessage to the refered actor.
            -- The message contains a reference to this actor.
            answerableMessage <$> self >>= send wr
        _ -> noop

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
  deriving (Show, Eq, Generic, Binary)

-- | Turnstile message
data TurnstileInput
    = Coin
    | Push
  deriving (Show, Eq, Generic, Binary)

instance Actor Turnstile where
    type Message Turnstile = TurnstileInput
    startState = Locked

    -- Unlock on Coin. Lock un Push
    behavior = \case
        (Right m) -> get >>= \case
            Locked -> case m of
                        Coin -> put Unlocked
                        Push -> return ()
            Unlocked -> case m of
                            Coin -> return ()
                            Push -> put Locked
        _ -> noop


-- | Actor with custom message type.
-- This one also communicates with another actor and expects a response.
newtype Msg = Msg String
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Convertible String Msg where
    convert = Msg

newtype OtherActor = OtherActor ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor OtherActor where
    type Message OtherActor = Msg
    type Creates OtherActor = '[WithRef]
    behavior = \case
        (Right _) -> do
            p <- create @WithRef
            a <- self
            p ! answerableMessage a
        _ -> noop



-- | Actor that handles references to other Actors
newtype WithRef = WithRef ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor WithRef where
    type Message WithRef = AnswerableMessage String
    behavior = \case
        (Right a) -> "hello" `answer` a
        _ -> noop


newtype Sender = Sender ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor Sender where
    type Message Sender = ActorRef Reciever
    behavior = \case
        (Right a) -> a ! "hello"
        _ -> noop

newtype Reciever = Reciever (Maybe String)
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor Reciever where
    type Message Reciever = String
    type Creates Reciever = '[Sender]
    behavior = \case
        (Left Created) -> do
            ref <- create @Sender
            me <- self
            ref ! me
        (Right m) -> put . Reciever . Just $ m
        _ -> noop

