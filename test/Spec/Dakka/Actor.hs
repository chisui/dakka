{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Spec.Dakka.Actor where

import           "base" Control.Monad                     (ap, void)
import           "base" Data.Monoid                       (Sum)
import           "base" Data.Proxy                        (Proxy (..))
import           "base" Data.Typeable                     (TypeRep, typeRep)
import           "base" GHC.Generics                      (Generic)
import           "base" Numeric.Natural                   (Natural)

import           "binary" Data.Binary                     (Binary)

import           "mtl" Control.Monad.State.Class          (MonadState (..))

import           "bytestring" Data.ByteString.Lazy        (pack)

import           "tasty" Test.Tasty                       (TestTree, testGroup)
import           "tasty-hunit" Test.Tasty.HUnit           (testCase, (@=?))
import           "tasty-quickcheck" Test.Tasty.QuickCheck (Arbitrary (..),
                                                           Positive (..), oneof,
                                                           testProperty, (===))

import           "dakka" Dakka.Actor.Internal
import           "dakka" Dakka.MockActorContext
import           "dakka" Dakka.Types

import           TestUtils                                (testMonoid)


newtype TrivialActor = TrivialActor ()
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)

instance Actor TrivialActor where
    type Message TrivialActor = ()


newtype CreatesActor = CreatesActor ()
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor CreatesActor where
    type Creates CreatesActor = '[TrivialActor]
    behavior _ = void $ create @TrivialActor
instance Arbitrary CreatesActor where
    arbitrary = pure mempty

newtype PlainMessageActor = PlainMessageActor ()
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor PlainMessageActor where
    type Message PlainMessageActor = String
    behavior = \case
        (Right "hello") -> noop
        _ -> noop


newtype CustomStateActor = CustomStateActor (Sum Int)
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary, Actor)

class CustomCapabillity (m :: * -> *) where
    custom :: m ()

newtype CustomCapabillitiesActor = CustomCapabillitiesActor ()
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor CustomCapabillitiesActor where
    type Capabillities CustomCapabillitiesActor = '[CustomCapabillity]
    behavior _ = custom


newtype GenericActor (a :: *) = GenericActor ()
    deriving stock    (Eq, Ord, Show, Read, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary, Actor)

data DummyContext a v = DummyContext
instance (DummyContext a `CanRunAll` a, Actor a) => ActorContext a (DummyContext a) where
    self      = DummyContext
    create' _ = DummyContext
    send _ _  = DummyContext
instance MonadState a (DummyContext a) where
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


newtype SignalWrapper (a :: [*]) = SignalWrapper
    { unwrapSignal :: Signal }
  deriving Eq
instance Show (SignalWrapper a) where show = show . unwrapSignal

instance Actor a => Arbitrary (ActorRef a) where
    arbitrary = ActorRef . pack <$> arbitrary

instance Arbitrary (SignalWrapper '[]) where
    arbitrary = pure $ SignalWrapper Created

instance (Actor a, Arbitrary a, Arbitrary (SignalWrapper as)) => Arbitrary (SignalWrapper (a ': as)) where
    arbitrary = SignalWrapper <$> oneof
        [ Obit <$> arbitrary @(ActorRef a)
               <*> arbitrary @a
        , unwrapSignal <$> arbitrary @(SignalWrapper as)
        ]


type SomeSignal = SignalWrapper '[CreatesActor]
type SomeRootActor = RootActor '[TrivialActor, PlainMessageActor]
type VoidDummyCtx = DummyContext (RootActor '[])

instance Arbitrary (RootActor l) where
    arbitrary = pure mempty

tests :: TestTree
tests = testGroup "Dakka.Actor"
    [ testGroup "Signal"
        [ testGroup "Eq"
            [ testProperty "a == a" $
                \ (s :: SomeSignal) -> unwrapSignal s === unwrapSignal s
            , testProperty "a == b" $
                \ (a :: SomeSignal) (b :: SomeSignal) -> case (unwrapSignal a, unwrapSignal b) of
                    (Created, Created)             -> a === b
                    (Obit refA' a', Obit refB' b') -> (a == b) === (refA' =~= refB' && a' =~= b')
                    _                              -> (a == b) === False
            ]
        , testProperty "Show" $
            \ (s :: SomeSignal) -> case unwrapSignal s of
                Obit refA a  -> show (unwrapSignal s) === "Obit " ++ show refA ++ " " ++ show a
                Created -> show (unwrapSignal s) === "Created <<CreatesActor>>"
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
                let [msg0, msg1] = snd $ execMock' $ behavior @SomeRootActor (Left Created)
                createdType msg0 @=? typeRep (Proxy @TrivialActor)
                createdType msg1 @=? typeRep (Proxy @PlainMessageActor)
            , testCase "run Created $ RootActor '[]" $
                snd (execMock' (behavior @(RootActor '[]) (Left Created))) @=? []
            ]
        ]
    ]

createdType :: SystemMessage -> TypeRep
createdType (Left (Creates r)) = typeRep r
createdType m = error $ "expected Creates SystemMessage but got" ++ show m

