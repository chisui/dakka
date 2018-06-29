{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main where

import "base" Data.Functor ( void )
import "base" Numeric.Natural ( Natural )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT )
import "transformers" Control.Monad.Trans.Class ( lift )

import "mtl" Control.Monad.State.Class ( MonadState(..) )

import "network-transport-tcp" Network.Transport.TCP (createTransport, defaultTCPParameters)
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Node

import qualified "dakka" Dakka.Actor as A
import "dakka" Dakka.Convert ( Convertible(..) )


newtype DistributedActorContext a v = DistributedActorContext (StateT a Process v)
  deriving (Functor, Applicative, Monad)

instance MonadState a (DistributedActorContext a) where
    state = DistributedActorContext . state

liftProcess :: Process v -> DistributedActorContext a v
liftProcess = DistributedActorContext . lift

instance Convertible Natural ProcessId where
    convert = 0
instance Convertible ProcessId Natural

instance (A.Actor a, MonadState a (DistributedActorContext a)) => A.ActorContext a (DistributedActorContext a) where
    self = A.ActorRef . convert <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (convert pid) m

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" ("localhost",) defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    void $ runProcess node $ do
        -- get our own process id
        self <- getSelfPid
        liftIO $ putStrLn $ "pid: " ++ show self
        send self "hello"
        hello <- expect @String
        liftIO $ putStrLn hello

