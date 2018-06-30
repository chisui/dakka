{-# OPTIONS_GHC -fno-warn-orphans #-} -- Arbitrary 
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Spec.Dakka.MockActorContext ( tests ) where

import "base" Data.Proxy ( Proxy(..) )

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
somePath = ActorRef mempty


instance Arbitrary (ActorRef a) where
    arbitrary = ActorRef . pack <$> arbitrary

tests :: TestTree
tests = testGroup "Dakka.MockActorContext"
    [ testGroup "SystemMessage"
        [ testGroup "Show"
            [ testCase "Create <<TrivialActor>>" $
                show (Create (Proxy @TrivialActor)) @=? "Create <<TrivialActor>>"
            , testCase "Create <<GenericActor Int>>" $
                show (Create (Proxy @(GenericActor Int))) @=? "Create <<GenericActor Int>>"
            , testCase "Send {to = (ctorRef <<TrivialActor>>@\"\"), msg = ()}" $
                show Send{ to  = ActorRef @TrivialActor mempty 
                         , msg = ()
                         } @=? "Send {to = (ActorRef <<TrivialActor>>@\"\"), msg = ()}"
            ]
        , testGroup "Eq"
            [ testCase "Create = Create" $
                Create (Proxy @TrivialActor) @=? Create (Proxy @TrivialActor)
            , testProperty "Send a b = Send a b" $
                \ (a :: SomePath) (a' :: SomePath) b b' -> (Send a b == Send a' b') === (a == a' && b == b')
            ]
        ]
    , testGroup "MockActorContext"
        [ testGroup "ActorContext"
            [ testCase "self" $
                evalMock' somePath self @=? somePath
            , testGroup "create"
                [ testCase "returns new path" $
                    evalMockRoot' @CreatesActor (create @TrivialActor)
                        @=? ActorRef mempty 
                , testCase "fires Create message" $
                    snd (execMockRoot' @CreatesActor (create @TrivialActor)) @=? [Create (Proxy @TrivialActor)]
                ]
            , testCase "send" $
                snd (execMock' somePath (self >>= (! "hello"))) @=? [Send somePath "hello"]
            ]
        , testGroup "MonadState"
            [ testCase "state" $
                fst (execMockRoot' (modify (\ (CustomStateActor i) -> CustomStateActor (i+1))))
                    @=? CustomStateActor 1
            ]
        , testCase "runMock somePath (pure ()) PlainMessageActor = ((), (PlainMessageActor, []))" $
            runMock somePath (pure ()) PlainMessageActor @=? ((), (PlainMessageActor, []))
        ]
    ]

