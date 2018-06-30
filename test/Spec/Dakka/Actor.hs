{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import "base" Numeric.Natural ( Natural )
import "base" Control.Monad ( void, ap )
import "base" Data.Proxy ( Proxy(..) )

import "binary" Data.Binary ( Binary )

import "mtl" Control.Monad.State.Class ( MonadState(..) )
import "bytestring" Data.ByteString.Lazy ( pack ) 

import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, (===), Arbitrary(..), oneof, Positive(..) )

import "dakka" Dakka.Actor
import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Path

import TestUtils ( testMonoid )


data TrivialActor = TrivialActor
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState TrivialActor
instance Actor TrivialActor where
    behavior = noop


data CreatesActor = CreatesActor
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState CreatesActor
instance Actor CreatesActor where
    type Creates CreatesActor = '[TrivialActor]
    behavior _ = void $ create @TrivialActor
instance Arbitrary CreatesActor where
    arbitrary = return CreatesActor

data PlainMessageActor = PlainMessageActor 
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState PlainMessageActor 
instance Actor PlainMessageActor where
    type Message PlainMessageActor = String
    onMessage "hello" = pure () 
    onMessage _       = pure () 
    onSignal = noop


newtype CustomStateActor = CustomStateActor Int
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState CustomStateActor where
    start = CustomStateActor 0
instance Actor CustomStateActor where
    behavior = noop


class CustomCapabillity (m :: * -> *) where
    custom :: m ()

data CustomCapabillitiesActor = CustomCapabillitiesActor 
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState CustomCapabillitiesActor 
instance Actor CustomCapabillitiesActor where
    type Capabillities CustomCapabillitiesActor = '[CustomCapabillity]
    behavior _ = custom


data GenericActor (a :: *) = GenericActor 
    deriving (Eq, Ord, Show, Read, Generic, Binary)
instance HasStartState (GenericActor a)
instance Typeable a => Actor (GenericActor a) where
    behavior = noop

data DummyContext a v = DummyContext
instance Actor a => ActorContext a (DummyContext a) where
    self      = DummyContext
    create' _ = DummyContext
    send _ _  = DummyContext
instance MonadState a (DummyContext a) where
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


instance Arbitrary Natural where
    arbitrary = fromInteger . getPositive <$> arbitrary

instance (Arbitrary a, Actor a) => Arbitrary (Signal (DummyContext a) a) where
  arbitrary = oneof
        [ pure Created
        , do aId <- pack <$> arbitrary
             a <- arbitrary
             pure $ Obit (ActorRef aId :: ActorRef a) a
        ]


type SomeSignal = Signal (DummyContext CreatesActor) CreatesActor
type SomeRootActor = RootActor '[TrivialActor, PlainMessageActor]
type VoidDummyCtx = DummyContext (RootActor '[])

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
                    (Created, Created)             -> a === b
                    (Obit refA' a', Obit refB' b') -> (a == b) === (refA' =~= refB' && a' =~= b')
                    _                              -> (a == b) === False
            ]
        , testProperty "Show" $
            \ (s :: SomeSignal) -> case s of
                Obit refA a  -> show s === "Obit " ++ show refA ++ " " ++ show a
                Created -> show s === "Created <<CreatesActor>>"
        ]
    , testGroup "RootActor"
        [ testMonoid @SomeRootActor
        , testGroup "Show"
            [ testCase "RootActor <<[TrivialActor,PlainMessageActor]>>" $
                show (mempty @SomeRootActor) @=? "RootActor <<[TrivialActor,PlainMessageActor]>>" 
            , testCase "RootActor <<[]>>" $
                show (mempty @(RootActor '[])) @=? "RootActor <<[]>>"
            ] 
        , testGroup "behavior"
            [ testCase "run Created $ RootActor '[TrivialActor, PlainMessageActor]" $ do
                let [msg0, msg1] = snd $ execMockRoot' $ onSignal @SomeRootActor Created
                msg0 @=? Create (Proxy @TrivialActor)
                msg1 @=? Create (Proxy @PlainMessageActor)
            , testCase "run Created $ RootActor '[]" $
                snd (execMockRoot' (onSignal @(RootActor '[]) Created)) @=? []
            ]
        ]
    ]


