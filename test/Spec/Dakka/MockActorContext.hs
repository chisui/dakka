{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Dakka.MockActorContext ( tests ) where

import "base" Data.Proxy ( Proxy(..) )

import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )

import Spec.Dakka.Actor hiding ( tests )

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor


tests :: TestTree
tests = testGroup "Dakka.MockActorContext"
    [ testGroup "SystemMessage"
        [ testGroup "Show"
            [ testCase "Create <<TrivialActor>>" $
                show (Create (Proxy @TrivialActor)) @=? "Create <<TrivialActor>>"
            , testCase "Create <<GenericActor Int>>" $
                show (Create (Proxy @(GenericActor Int))) @=? "Create <<GenericActor Int>>"
            , testCase "Send {to = (ActorPath /TrivialActor:any/), msg = (PlainMessage ())}" $
                show Send{ to  = ActorPath (root @TrivialActor)
                         , msg = PlainMessage ()
                         } @=? "Send {to = (ActorPath /TrivialActor:any/), msg = (PlainMessage ())}"
            ]
        ]
    ]

