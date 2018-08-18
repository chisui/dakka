{-# OPTIONS_GHC -fno-warn-orphans #-} -- Arbitrary 
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Spec.Dakka.MockActorContext ( tests ) where

import "mtl" Control.Monad.State.Class ( modify )

import "bytestring" Data.ByteString.Lazy ( pack ) 

import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, (===), Arbitrary(..) )

import Spec.Dakka.Actor hiding ( tests )
import Spec.Dakka.PathArbitrary ()

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor


type SomePath = ActorRef PlainMessageActor

somePath :: SomePath
somePath = ActorRef "0" 


instance Arbitrary (ActorRef a) where
    arbitrary = ActorRef . pack <$> arbitrary

tests :: TestTree
tests = testGroup "Dakka.MockActorContext"
    [ testGroup "SystemMessage"
        [ testGroup "Show"
            [ testCase "Creates <<TrivialActor>>@\"id\"" $
                show (Creates (ActorRef @TrivialActor "id")) @=? "Creates <<TrivialActor>>@\"id\""
            , testCase "Creates <<GenericActor Int>>" $
                show (Creates (ActorRef @(GenericActor Int) "id")) @=? "Creates <<GenericActor Int>>@\"id\""
            , testCase "Send {to = (ctorRef <<TrivialActor>>@\"\"), msg = ()}" $
                show (Send (ActorRef @TrivialActor mempty) ())
                    @=? "Send {to = (ActorRef <<TrivialActor>>@\"\"), msg = ()}"
            ]
        , testGroup "Eq"
            [ testCase "Creates = Creates" $
                Creates (ActorRef @TrivialActor "") @=? Creates (ActorRef @TrivialActor "")
            , testProperty "Send a b = Send a b" $
                \ (a :: SomePath) (a' :: SomePath) b b' -> (Send a b == Send a' b') === (a == a' && b == b')
            ]
        ]
    , testGroup "MockActorContext"
        [ testGroup "ActorContext"
            [ testCase "self" $
                evalMock' self @=? somePath
            , testGroup "create"
                [ testCase "returns new path" $
                    evalMock' @CreatesActor (create @TrivialActor)
                        @=? ActorRef "0" 
                , testCase "fires Create message" $
                    snd (execMock' @CreatesActor (create @TrivialActor)) @=? [Creates (ActorRef @TrivialActor "1")]
                ]
            , testCase "send" $
                snd (execMock' @PlainMessageActor (self >>= (! "hello"))) @=? [Send somePath "hello"]
            ]
        , testGroup "MonadState"
            [ testCase "state" $
                fst (execMock' (modify (\ (CustomStateActor i) -> CustomStateActor (i+1))))
                    @=? CustomStateActor 1
            ]
        , testCase "runMock somePath (pure ()) PlainMessageActor = ((), (PlainMessageActor, []))" $
            runMock (pure ()) PlainMessageActor @=? ((), (PlainMessageActor, []))
        ]
    ]

