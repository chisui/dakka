{-# LANGUAGE PackageImports
           , FlexibleInstances
           , MultiParamTypeClasses
           , DeriveDataTypeable
           , DataKinds
           , TypeApplications
           , TypeFamilies
           , TypeSynonymInstances
#-}
module TestActors where

import "base" Data.Typeable

import "dakka" Dakka.Actor
import "dakka" Dakka.AnswerableMessage
import "dakka" Dakka.Convert

import "base" Control.Monad.IO.Class ( MonadIO( liftIO ) )
import "mtl" Control.Monad.State.Class ( modify )


-- | Actor with all bells and whistles.
newtype TestActor = TestActor
    { i :: Int
    } deriving (Show, Eq, Typeable)

instance Semigroup TestActor where
    (TestActor i) <> (TestActor j) = TestActor (i + j)

instance Monoid TestActor where
    mempty = TestActor 0

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
newtype Msg = Msg String deriving (Show, Eq, Typeable)
instance Convertible String Msg where
    convert = Msg

data OtherActor = OtherActor deriving (Show, Eq, Typeable)

instance Actor OtherActor where
    type Message OtherActor = Msg
    type Creates OtherActor = '[WithRef]
    behavior m = do
        p <- create @WithRef
        a <- self
        p ! AnswerableMessage a

    startState = OtherActor


-- | Actor that handles references to other Actors
data WithRef = WithRef deriving (Show, Eq, Typeable)
instance Actor WithRef where
    type Message WithRef = AnswerableMessage String
    behavior = answer "hello"
    startState = WithRef

