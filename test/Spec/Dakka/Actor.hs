{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module Spec.Dakka.Actor where

import "base" GHC.Generics ( Generic )
import "base" Data.Typeable ( Typeable )
import "base" Control.Monad ( void, ap )
import "base" Data.Proxy ( Proxy(..) )
import "base" Control.Applicative ( Const(..) )
import "base" Data.Void ( Void )

import "mtl" Control.Monad.State.Class ( MonadState(..) )

import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, (===), Arbitrary(..), oneof )

import "dakka" Dakka.Actor
import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Path

import TestUtils ( testMonoid )


data TrivialActor = TrivialActor
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState TrivialActor
instance Actor TrivialActor where
    behavior = noop


data CreatesActor = CreatesActor
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState CreatesActor
instance Actor CreatesActor where
    type Creates CreatesActor = '[TrivialActor]
    behavior _ = void $ create @TrivialActor


data PlainMessageActor = PlainMessageActor 
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState PlainMessageActor 
instance Actor PlainMessageActor where
    type Message PlainMessageActor = PlainMessage String
    onMessage (PlainMessage "hello") = pure () 
    onMessage _                      = pure () 
    onSignal = noop


data CustomMessage ref (p :: Path *) = CustomMessage
    { usageOfRef :: ref ('Root CreatesActor ':/ TrivialActor)
    , usageOfP   :: Proxy p
    } deriving (Generic, Typeable)
instance ActorRef ref => Eq (CustomMessage ref p) where
    (CustomMessage r _) == (CustomMessage r' _) = r `eqRef` r'
instance Show (CustomMessage ref p) where
    show _ = "CustomMessage"
instance ActorMessage CustomMessage

data CustomMessageActor = CustomMessageActor 
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState CustomMessageActor 
instance Actor CustomMessageActor where
    type Message CustomMessageActor = CustomMessage
    onMessage (CustomMessage _ _) = pure () 
    onSignal = noop


class CustomCapabillity (m :: * -> *) where
    custom :: m ()

data CustomCapabillitiesActor = CustomCapabillitiesActor 
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState CustomCapabillitiesActor 
instance Actor CustomCapabillitiesActor where
    type Capabillities CustomCapabillitiesActor = '[CustomCapabillity]
    behavior _ = custom


data GenericActor (a :: *) = GenericActor 
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance HasStartState (GenericActor a)
instance Typeable a => Actor (GenericActor a) where
    behavior = noop

instance (Typeable a, Eq a, Show a) => ActorRef (Const a) 

instance Arbitrary m => Arbitrary (PlainMessage m (r ::k0) (p ::k1)) where
    arbitrary = PlainMessage <$> arbitrary

data DummyContext (p :: Path *) a = DummyContext
instance (MonadState (Tip p) (DummyContext p), ActorRefConstraints p) => ActorContext p (DummyContext p) where
    type Ref (DummyContext p) = Const Bool 
    self      = DummyContext
    create' _ = DummyContext
    send _ _  = DummyContext
instance (Tip ('Root a) ~ a) => MonadState a (DummyContext ('Root a)) where
    state _ = DummyContext
instance (Tip (as ':/ a) ~ a) => MonadState a (DummyContext (as ':/ a)) where
    state _ = DummyContext
instance Functor (DummyContext p) where
    fmap _ _ = DummyContext
instance Applicative (DummyContext p) where
    pure _ = DummyContext
    (<*>) = ap
instance Monad (DummyContext p) where
    return  = pure
    _ >>= _ = DummyContext
instance Arbitrary (DummyContext p a) where
    arbitrary = pure DummyContext

instance (Tip p ~a, ActorRefConstraints p, TrivialActor :∈ Creates a) => Arbitrary (Signal (DummyContext p) a) where
  arbitrary = oneof
        [ pure Created
        , do b <- arbitrary
             pure $ Obit (Const b :: Const Bool (p ':/ TrivialActor))
        ]


type SomeSignal = Signal (DummyContext ('Root CreatesActor)) CreatesActor
type SomeRootActor = RootActor '[TrivialActor, CustomMessageActor]

instance Arbitrary (RootActor l) where
    arbitrary = pure mempty

tests :: TestTree
tests = testGroup "Dakka.Actor"
    [ testGroup "Signal"
        [ testGroup "Eq"
            [ testProperty "a == a" $
                \ (s :: SomeSignal) -> s === s
            , testProperty "a == b" $
                \ (a :: SomeSignal) (b :: SomeSignal) -> case (a, b) of
                    (Created, Created) -> a === b
                    (Obit a', Obit b') -> (a == b) === (a' `eqRef` b')
                    _                  -> (a == b) === False
            ]
        , testProperty "Show" $
            \ (s :: SomeSignal) -> case s of
                Obit r  -> show s === "Obit (" ++ show r ++ ")"
                Created -> show s === "Created <<CreatesActor>>"
        ]
    , testGroup "PlainMessage"
        [ testProperty "read . show = id" $
            \ (m :: PlainMessage (Maybe Bool) Void Void) -> read (show m) === m
        , testGroup "Show"
            [ testCase "PlainMessage 42" $ do
                show (PlainMessage 42) @=? "PlainMessage 42"
                show PlainMessage{getPlainMessage = 42} @=? "PlainMessage 42"
            , testCase "PlainMessage (Just 42)" $
                show (PlainMessage (Just 42)) @=? "PlainMessage (Just 42)"
            , testCase "Just (PlainMessage 42)" $
                show (Just (PlainMessage 42)) @=? "Just (PlainMessage 42)"
            ]
        , testGroup "ActorMessage"
            [ testProperty "eqMsg = (==)" $
                \ (a :: PlainMessage Bool (Const Bool) ('Root Void)) b -> (a `eqMsg` b) === (a == b)
            , testProperty "showsPrec = showsMsg" $
                \ (a :: PlainMessage (Maybe Bool) (Const Bool) ('Root Void)) str b 
                    -> let d = if b then 5 else 11
                        in showsPrec d a str === showsMsg d a str
            ]
        ]
    , testGroup "RootActor"
        [ testMonoid @SomeRootActor
        , testGroup "Show"
            [ testCase "RootActor <<[TrivialActor,CustomMessageActor]>>" $
                show (mempty @SomeRootActor) @=? "RootActor <<[TrivialActor,CustomMessageActor]>>" 
            , testCase "RootActor <<[]>>" $
                show (mempty @(RootActor '[])) @=? "RootActor <<[]>>"
            ] 
        , testGroup "behavior"
            [ testCase "run Created $ RootActor '[TrivialActor, CustomMessageActor]" $ do
                let [msg0, msg1] = snd $ execMockRoot' $ onSignal @SomeRootActor Created
                msg0 @=? Create (Proxy @TrivialActor)
                msg1 @=? Create (Proxy @CustomMessageActor)
            , testCase "run Created $ RootActor '[]" $
                snd (execMockRoot' (onSignal @(RootActor '[]) Created)) @=? []
            ]
        ]
    ]


