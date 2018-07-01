{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import "base" Data.Functor ( void )
import "base" Data.Proxy ( Proxy )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Class ( lift )

import "mtl" Control.Monad.State.Class ( MonadState(..) )

import "network-transport" Network.Transport ( Transport )
import "network-transport-tcp" Network.Transport.TCP ( createTransport, defaultTCPParameters )
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Node

import "binary" Data.Binary ( encode, decode )

import qualified "dakka" Dakka.Actor as A
import "dakka" Dakka.Constraints


newtype DistributedActorContext a v = DistributedActorContext (StateT a Process v)
  deriving (Functor, Applicative, Monad)

instance MonadState a (DistributedActorContext a) where
    state = DistributedActorContext . state

liftProcess :: Process v -> DistributedActorContext a v
liftProcess = DistributedActorContext . lift

expectMessage :: (A.Actor a, ImplementsAll (DistributedActorContext a) (A.Capabillities a)) => DistributedActorContext a ()
expectMessage = A.behavior =<< Right <$> liftProcess expect

instance A.Actor a => A.ActorContext a (DistributedActorContext a) where
    self = A.ActorRef . encode <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (decode pid) m
    create' :: forall b. ( A.Actor b, b :∈ A.Creates a ) => Proxy b -> DistributedActorContext a (A.ActorRef b) 
    create' _ = liftProcess $ do
        nodeId <- getSelfNode -- only spawn on this node for now. Maybe create some kind of loadbalancing mechanism?
        let staticRunActor = undefined
        let b = A.startState @b
        pid <- spawn nodeId (staticRunActor b)
        return . A.ActorRef . encode $ pid

main :: IO ()
main = do
    t <- createSimpleTransport
    node <- newLocalNode t initRemoteTable
    void $ runProcess node $ do
        -- get our own process id
        self <- getSelfPid
        liftIO $ putStrLn $ "pid: " ++ show self
        send self "hello"
        hello <- expect @String
        liftIO $ putStrLn hello

createSimpleTransport :: IO Transport
createSimpleTransport = do
    Right t <- createTransport "127.0.0.1" "10501" ("localhost",) defaultTCPParameters
    return t

