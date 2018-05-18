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
import "base" Data.Typeable ( Typeable )
import "base" Control.Applicative ( Const(..) )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, runStateT )
import "transformers" Control.Monad.Trans.Reader ( ReaderT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )
import "transformers" Control.Monad.Trans.Class ( lift )

import "mtl" Control.Monad.State.Class ( MonadState(..) )
import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )

import "network-transport-tcp" Network.Transport.TCP (createTransport, defaultTCPParameters)
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Node

import qualified "dakka" Dakka.Actor as A
import "dakka" Dakka.Path


type ActorPid (p :: Path *) = Const ProcessId p
instance A.ActorRef (Const ProcessId) 

newtype DistributedActorContext (p :: Path *) a = DistributedActorContext (StateT (Tip p) Process a)
  deriving (Functor, Applicative, Monad)

instance (a ~ Tip p) => MonadState a (DistributedActorContext p) where
    state = DistributedActorContext . state

liftProcess = DistributedActorContext . lift

instance Typeable p => A.ActorRef (A.CtxRef (DistributedActorContext p))

instance ( A.ActorRefConstraints p
         , MonadState (Tip p) (DistributedActorContext p)
         ) => A.ActorContext (DistributedActorContext p) where
    data CtxRef  (DistributedActorContext p) q = ActorId ProcessId
        deriving (Eq, Show)
    type CtxPath (DistributedActorContext p) = p 
    self = ActorId <$> liftProcess getSelfPid
    (ActorId pid) ! m = liftProcess $ send pid m

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

