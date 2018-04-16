{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module Spec.Dakka.Actor where

import "base" GHC.Generics ( Generic )
import "base" Data.Typeable ( Typeable )
import "base" Control.Monad ( void )
import "base" Data.Proxy ( Proxy(..) )

import "tasty" Test.Tasty ( testGroup, TestTree )

import "dakka" Dakka.Actor
import "dakka" Dakka.Path


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


tests :: TestTree
tests = testGroup "Dakka.Actor" []

