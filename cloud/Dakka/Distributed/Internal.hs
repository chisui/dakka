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


newtype DistributedActorContext r a v
    = DistributedActorContext
        { runDAC :: (StateT a Process v)
        }
  deriving (Functor, Applicative, Monad)

instance MonadState a (DistributedActorContext r a) where
    state = DistributedActorContext . state

liftProcess :: Process v -> DistributedActorContext r a v
liftProcess = DistributedActorContext . lift

expectMessage :: ( r `A.IsRootOf` a
                 , m `A.CanRunAll` r
                 ) => DistributedActorContext r a ()
expectMessage = A.behavior =<< Right <$> liftProcess expect

instance ( r `A.IsRootOf` a
         , m `A.CanRunAll` r
         ) => A.ActorContext r a (DistributedActorContext r a) where
    self = A.ActorRef . encode <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (decode pid) m

instance MonadIO (DistributedActorContext r a) where
    liftIO = DistributedActorContext . lift . liftIO

staticRunActor :: forall a r. (A.Actor a, DistributedActorContext r a `A.HasAllCapabillities` a) => Closure (Process ())
staticRunActor = error "cant run static actor"

runActor :: forall a r. (A.Actor a, DistributedActorContext r a `A.HasAllCapabillities` a) => Process ()
runActor = void . runStateT (runDAC (initActor *> forever awaitMessage)) $ A.startState @a
  where
    initActor = A.behavior . Left $ A.Created
    awaitMessage = A.behavior . Right =<<liftProcess (expect @(A.Message a))

createActorSystem :: forall a. (A.Actor a, DistributedActorContext a a `A.HasAllCapabillities` a) => Transport -> IO ()
createActorSystem t = do
    node <- newLocalNode t initRemoteTable
    runProcess node (runActor @a)


