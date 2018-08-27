{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Main (main) where

import           "base" Control.Applicative                    (liftA2)
import           "base" Control.Monad                          (join)
import           "base" Control.Monad.IO.Class                 (MonadIO, liftIO)
import           "base" Data.Proxy                             (Proxy (..))
import           "base" GHC.Generics                           (Generic)

import           "mtl" Control.Monad.State.Class               (MonadState, get)

import           "network-transport-tcp" Network.Transport.TCP (createTransport, defaultTCPParameters)

import           "binary" Data.Binary                          (Binary)

import           "dakka" Dakka.Actor                           (Actor, ActorRef,
                                                                Capabillities,
                                                                Creates,
                                                                Message,
                                                                RootActor,
                                                                Signal (Created),
                                                                behavior,
                                                                create, noop,
                                                                self, (!))

import           Dakka.Distributed                             (createActorSystem)


-- Trivial Actors

newtype ActorA = ActorA ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor ActorA where
    type Capabillities ActorA = '[MonadIO]
    behavior = logMessage

newtype ActorB = ActorB ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor ActorB where
    type Capabillities ActorB = '[MonadIO]
    behavior = logMessage

logMessage :: forall a b m. (MonadIO m, MonadState a m, Show a, Show b) => b -> m ()
logMessage b = do
    s <- get
    liftIO . putStrLn $ show s ++ " recieved " ++ show  b

-- Simple reference passing actors

newtype SimpleReciever = SimpleReciever ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor SimpleReciever where
    type Message SimpleReciever = String
    type Creates SimpleReciever = '[SimpleSender]
    type Capabillities SimpleReciever = '[MonadIO]

    behavior = \case
        (Left Created) -> join $ liftA2 (!) (create @SimpleSender) self
        (Right m) -> logMessage m
        _ -> noop

newtype SimpleSender = SimpleSender ()
    deriving stock    (Eq, Show, Generic)
    deriving newtype  (Semigroup, Monoid)
    deriving anyclass (Binary)
instance Actor SimpleSender where
    type Message SimpleSender = ActorRef SimpleReciever

    behavior = \case
        (Right m) -> m ! "msg"
        _ -> noop

-- Main

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" ("localhost",) defaultTCPParameters
    createActorSystem t (Proxy @(RootActor '[ ActorA, ActorB ]))

