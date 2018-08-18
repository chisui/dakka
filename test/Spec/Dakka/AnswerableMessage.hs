{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Spec.Dakka.AnswerableMessage ( tests ) where

import "base" Data.Typeable ( cast )
import "base" Control.Applicative ( liftA2 )
import "base" GHC.Generics ( Generic )

import "binary" Data.Binary ( Binary )

import "tasty" Test.Tasty ( TestTree, testGroup )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )

import "dakka" Dakka.Actor
import "dakka" Dakka.AnswerableMessage
import "dakka" Dakka.MockActorContext


-- | Actor that always answers "hello"
data SimpleAnsweringActor = SimpleAnsweringActor
    deriving (Eq, Show, Generic, Binary)
instance HasStartState SimpleAnsweringActor
instance Actor SimpleAnsweringActor where
    type Message SimpleAnsweringActor = AnswerableMessage String
    onMessage = answer "hello"
    onSignal = noop

-- | Actor that creates a SimpleAnsweringActor on creation and asks it a question
data SimpleAskingActor = SimpleAskingActor
    deriving (Eq, Show, Generic, Binary)
instance HasStartState SimpleAskingActor
instance Actor SimpleAskingActor where
    type Message SimpleAskingActor = String
    type Creates SimpleAskingActor = '[SimpleAnsweringActor]
    behavior (Left Created) = do
        req <- answerableMessage <$> self
        create @SimpleAnsweringActor >>= (! req)
    behavior _ = pure ()


tests :: TestTree
tests = testGroup "Dakka.AnswerableMessage"
    [ testCase "ask and answer chain" $ do

        let [_, askAction] = snd $ execMock' @SimpleAskingActor (onSignal Created)

        let (Just (_, askMsg)) = unwrapAskAction askAction

        let [answerAction] = snd $ execMock' @SimpleAnsweringActor (onMessage askMsg)

        let (Just (_, answerMsg)) = unwrapAnswerAction answerAction

        answerMsg @=? "hello"
    ]

unwrapSend :: forall a. Actor a => SystemMessage -> Maybe (ActorRef a, Message a)
unwrapSend (Send r m) = liftA2 (,) (cast r) (cast m)
unwrapSend _ = Nothing

unwrapAskAction :: SystemMessage -> Maybe (ActorRef SimpleAnsweringActor, AnswerableMessage String)
unwrapAskAction = unwrapSend

unwrapAnswerAction :: SystemMessage -> Maybe (ActorRef SimpleAskingActor, String)
unwrapAnswerAction = unwrapSend

