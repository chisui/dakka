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

import           "transformers" Control.Monad.Trans.Class               (lift)
import           "transformers" Control.Monad.Trans.State.Lazy          (StateT, runStateT)

import           "mtl" Control.Monad.State.Class                        (MonadState (..))

import           "network-transport" Network.Transport                  (Transport)

import           "distributed-process" Control.Distributed.Process      (Closure,
                                                                         Process,
                                                                         expect,
                                                                         getSelfPid,
                                                                         processNodeId,
                                                                         send,
                                                                         spawn)
import           "distributed-process" Control.Distributed.Process.Node (initRemoteTable,
                                                                         newLocalNode,
                                                                         runProcess)

import           "binary" Data.Binary                                   (decode,
                                                                         encode)

import qualified "dakka" Dakka.Actor.Internal                           as A


newtype DistributedActorContext a v
    = DistributedActorContext
        { runDAC :: StateT a Process v
        }
  deriving (Functor, Applicative, Monad, MonadState a, MonadIO)

liftProcess :: Process v -> DistributedActorContext a v
liftProcess = DistributedActorContext . lift

expectMessage :: forall a. (A.Actor a, DistributedActorContext a `A.CanRunAll` a) => DistributedActorContext a ()
expectMessage = A.behavior =<< Right <$> liftProcess (expect @(A.Message a))

instance (DistributedActorContext a `A.CanRunAll` a) => A.ActorContext a (DistributedActorContext a) where
    self = A.ActorRef . encode <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (decode pid) m

    create' a = liftProcess $ do
        nid <- processNodeId <$> getSelfPid
        pid <- spawn nid (staticRunActor a)
        return . A.ActorRef . encode $ pid

staticRunActor :: forall a proxy. A.Actor a => proxy a -> Closure (Process ())
staticRunActor = error "cant run static actor"

runActor :: forall a proxy. (A.Actor a, DistributedActorContext a `A.CanRunAll` a) => proxy a -> Process ()
runActor _ = void . runStateT (runDAC (initActor *> forever awaitMessage)) $ A.startState @a
  where
    initActor = A.behavior . Left $ A.Created
    awaitMessage = A.behavior . Right =<< liftProcess (expect @(A.Message a))

createActorSystem :: forall a proxy. (A.Actor a, DistributedActorContext a `A.CanRunAll` a) => Transport -> proxy a -> IO ()
createActorSystem t p = do
    node <- newLocalNode t initRemoteTable
    runProcess node (runActor p)


