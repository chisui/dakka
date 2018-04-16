{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Spec.Dakka.AnswerableMessage ( tests ) where

import "base" Data.Typeable ( Typeable, cast )
import "base" GHC.Generics ( Generic )

import "tasty" Test.Tasty ( TestTree, testGroup )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )

import "dakka" Dakka.Actor
import "dakka" Dakka.Path
import "dakka" Dakka.AnswerableMessage
import "dakka" Dakka.MockActorContext


-- | Actor that always answers "hello"
data SimpleAnsweringActor = SimpleAnsweringActor
    deriving (Eq, Show, Typeable, Generic)
instance HasStartState SimpleAnsweringActor
instance Actor SimpleAnsweringActor where
    type Message SimpleAnsweringActor = AnswerableMessage (PlainMessage String)
    onMessage = answer $ PlainMessage "hello"
    onSignal = noop

-- | Actor that creates a SimpleAnsweringActor on creation and asks it a question
data SimpleAskingActor = SimpleAskingActor
    deriving (Eq, Show, Typeable, Generic)
instance HasStartState SimpleAskingActor
instance Actor SimpleAskingActor where
    type Message SimpleAskingActor = PlainMessage String
    type Creates SimpleAskingActor = '[SimpleAnsweringActor]
    behavior (Left Created) = do
        req <- AnswerableMessage <$> self
        create @SimpleAnsweringActor >>= (! req)
    behavior _ = pure ()


tests :: TestTree
tests = testGroup "Dakka.AnswerableMessage"
    [ testCase "ask and answer chain" $ do

        let askingRef = ActorPath $ root @SimpleAskingActor

        let [_, askAction] = snd $ execMock' askingRef (onSignal Created)

        let (Just (answeringRef, askMsg)) = unwrapAskAction askAction

        let [answerAction] = snd $ execMock' answeringRef (onMessage askMsg)

        let (Just (askingRef', answerMsg)) = unwrapAnswerAction answerAction

        askingRef @=? askingRef'
        answerMsg @=? PlainMessage "hello"
    ]

unwrapAskAction :: SystemMessage
                -> Maybe ( ActorPath ('Root SimpleAskingActor ':/ SimpleAnsweringActor)
                         , AnswerableMessage (PlainMessage String) ActorPath ('Root SimpleAskingActor ':/ SimpleAnsweringActor)
                         )
unwrapAskAction (Send r m) = do
    r' <- cast r 
    m' <- cast m
    pure (r', m')
unwrapAskAction _ = Nothing

unwrapAnswerAction :: SystemMessage
                   -> Maybe ( ActorPath ('Root SimpleAskingActor)
                            , PlainMessage String ActorPath ('Root SimpleAskingActor)
                            )
unwrapAnswerAction (Send r m) = do
    r' <- cast r 
    m' <- cast m
    pure (r', m')
unwrapAnswerAction _ = Nothing

