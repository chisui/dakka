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


import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, (===), Arbitrary(..) )

import Spec.Dakka.Actor hiding ( tests )
import Spec.Dakka.PathArbitrary ()

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor
import "dakka" Dakka.Path


type SomePath = CtxRef (MockActorContext ('Root PlainMessageActor)) ('Root PlainMessageActor)

somePath :: SomePath
somePath = ActorPath $ root @PlainMessageActor


instance Arbitrary (HPathT p ()) => Arbitrary (CtxRef (MockActorContext p) p) where
    arbitrary = ActorPath <$> arbitrary

tests :: TestTree
tests = testGroup "Dakka.MockActorContext"
    [ testGroup "SystemMessage"
        [ testGroup "Show"
            [ testCase "Create <<TrivialActor>>" $
                show (Create (Proxy @TrivialActor)) @=? "Create <<TrivialActor>>"
            , testCase "Create <<GenericActor Int>>" $
                show (Create (Proxy @(GenericActor Int))) @=? "Create <<GenericActor Int>>"
            , testCase "Send {to = (ActorPath /TrivialActor:()/), msg = (PlainMessage ())}" $
                show Send{ to  = ActorPath (root @TrivialActor)
                         , msg = PlainMessage @() @(MockActorContext ('Root TrivialActor)) ()
                         } @=? "Send {to = (ActorPath /TrivialActor:()/), msg = (PlainMessage ())}"
            ]
        , testGroup "Eq"
            [ testCase "Create = Create" $
                Create (Proxy @TrivialActor) @=? Create (Proxy @TrivialActor)
            , testProperty "Send a b = Send a b" $
                \ (a :: SomePath) (a' :: SomePath) b b' -> (Send a b == Send a' b') === (a == a' && b == b')
            ]
        ]
    , testGroup "ActorPath"
        [ testGroup "Eq"
            [ testProperty "ActorPath p == ActorPath q = p == q" $
                \ (p :: HPathT ('Root ()) ()) q ->  (ActorPath p == ActorPath q) === (p == q)
            , testProperty "a == b = a `eqRef` b" $
                \ (a :: SomePath) b -> (a == b) === (a `eqRef` b)
            ]
        , testGroup "Show"
            [ testProperty "ActorPath /():id/" $
                \ (p :: HPathT ('Root ()) ()) -> show (ActorPath p) === ("ActorPath " ++ show p)
            , testProperty "ActorPath /():id/():id/" $
                \ (p :: HPathT ('Root () ':/ ()) ()) -> show (ActorPath p) === ("ActorPath " ++ show p)
            , testProperty "showsPrec = showsRef" $
                \ (p :: SomePath) s b -> let d = if b then 5 else 11
                                          in showsPrec d p s === showsRef d p s
            ]
        ]
    , testGroup "MockActorContext"
        [ testGroup "ActorContext"
            [ testCase "self" $
                evalMock' somePath self @=? somePath
            , testGroup "create"
                [ testCase "returns new path" $
                    evalMockRoot' @CreatesActor (create @TrivialActor)
                        @=? ActorPath (root @CreatesActor </> Proxy @TrivialActor) 
                , testCase "fires Create message" $
                    snd (execMockRoot' @CreatesActor (create @TrivialActor)) @=? [Create (Proxy @TrivialActor)]
                ]
            , testCase "send" $
                snd (execMock' somePath (self >>= (! PlainMessage "hello"))) @=? [Send somePath (PlainMessage "hello")]
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

