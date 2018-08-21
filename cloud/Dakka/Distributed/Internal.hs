{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Dakka.Distributed.Internal where

import           "base" Control.Monad                                   (forever)
import           "base" Control.Monad.IO.Class                          (MonadIO,
                                                                         liftIO)
import           "base" Data.Functor                                    (void)
import           "base" Data.Proxy                                      (Proxy)

import           "transformers" Control.Monad.Trans.Class               (lift)
import           "transformers" Control.Monad.Trans.State.Lazy          (StateT, runStateT)

import           "mtl" Control.Monad.State.Class                        (MonadState (..))

import           "network-transport" Network.Transport                  (Transport)

import           "distributed-process" Control.Distributed.Process      (Closure,
                                                                         Process,
                                                                         expect,
                                                                         getSelfNode,
                                                                         getSelfPid,
                                                                         send,
                                                                         spawn)
import           "distributed-process" Control.Distributed.Process.Node (initRemoteTable,
                                                                         newLocalNode,
                                                                         runProcess)

import           "binary" Data.Binary                                   (decode,
                                                                         encode)

import qualified "dakka" Dakka.Actor                                    as A
import           "dakka" Dakka.Constraints


newtype DistributedActorContext a v
    = DistributedActorContext
        { runDAC :: (StateT a Process v)
        }
  deriving (Functor, Applicative, Monad)

instance MonadState a (DistributedActorContext a) where
    state = DistributedActorContext . state

liftProcess :: Process v -> DistributedActorContext a v
liftProcess = DistributedActorContext . lift

expectMessage :: (A.Actor a, DistributedActorContext a `A.HasAllCapabillities` a) => DistributedActorContext a ()
expectMessage = A.behavior =<< Right <$> liftProcess expect

instance (A.Actor a, DistributedActorContext a `A.HasAllCapabillities` a) => A.ActorContext a (DistributedActorContext a) where
    self = A.ActorRef . encode <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (decode pid) m
    create' :: forall b.
      ( A.Actor b
      , b :∈ A.Creates a
      , DistributedActorContext a `A.HasAllCapabillities` b
      ) => Proxy b -> DistributedActorContext a (A.ActorRef b)
    create' _ = liftProcess $ do
        nodeId <- getSelfNode -- only spawn on this node for now. Maybe create some kind of loadbalancing mechanism?
        pid <- spawn nodeId (staticRunActor @b)
        return . A.ActorRef . encode $ pid

instance MonadIO (DistributedActorContext a) where
    liftIO = DistributedActorContext . lift . liftIO

staticRunActor :: forall a. (A.Actor a, DistributedActorContext a `A.HasAllCapabillities` a) => Closure (Process ())
staticRunActor = error "cant run static actor"

runActor :: forall a. (A.Actor a, DistributedActorContext a `A.HasAllCapabillities` a) => Process ()
runActor = void . runStateT (runDAC (initActor *> forever awaitMessage)) $ A.startState @a
  where
    initActor = A.behavior . Left $ A.Created
    awaitMessage = A.behavior . Right =<<liftProcess (expect @(A.Message a))

createActorSystem :: forall a. (A.Actor a, DistributedActorContext a `A.HasAllCapabillities` a) => Transport -> IO ()
createActorSystem t = do
    node <- newLocalNode t initRemoteTable
    runProcess node (runActor @a)


