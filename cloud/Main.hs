{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

import           "base" Control.Applicative                    (liftA2)
import           "base" Control.Monad                          (join)
import           "base" Control.Monad.IO.Class                 (MonadIO, liftIO)
import           "base" GHC.Generics                           (Generic)

import           "mtl" Control.Monad.State.Class               (MonadState, get)

import           "network-transport-tcp" Network.Transport.TCP (createTransport, defaultTCPParameters)

import           "binary" Data.Binary                          (Binary)

import           "dakka" Dakka.Actor                           (Actor, ActorRef,
                                                                Capabillities,
                                                                Creates,
                                                                HasStartState,
                                                                Message,
                                                                RootActor,
                                                                Signal (Created),
                                                                behavior,
                                                                create,
                                                                onMessage,
                                                                onSignal, self,
                                                                (!))

import           Dakka.Distributed                             (createActorSystem)


-- Trivial Actors

data ActorA = ActorA deriving ( Eq, Show, Generic, Binary, HasStartState )
instance Actor ActorA where
    type Capabillities ActorA = '[MonadIO]
    behavior = logMessage

data ActorB = ActorB deriving ( Eq, Show, Generic, Binary, HasStartState )
instance Actor ActorB where
    type Capabillities ActorB = '[MonadIO]
    behavior = logMessage

logMessage :: forall a b m. (MonadIO m, MonadState a m, Show a, Show b) => b -> m ()
logMessage b = do
    s <- get
    liftIO . putStrLn $ show s ++ " recieved " ++ show  b

-- Simple reference passing actors

data SimpleReciever = SimpleReciever deriving ( Eq, Show, Generic, Binary, HasStartState )
instance Actor SimpleReciever where
    type Message SimpleReciever = String
    type Creates SimpleReciever = '[SimpleSender]
    type Capabillities SimpleReciever = '[MonadIO]

    onSignal Created = join $ liftA2 (!) (create @SimpleSender) self
    onSignal _       = pure ()
    onMessage = logMessage

data SimpleSender = SimpleSender deriving ( Eq, Show, Generic, Binary, HasStartState )
instance Actor SimpleSender where
    type Message SimpleSender = ActorRef SimpleReciever
    onMessage = (! "msg")
    onSignal _ = pure ()

-- Main

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" ("localhost",) defaultTCPParameters
    createActorSystem @(RootActor '[ ActorA, ActorB ]) t

