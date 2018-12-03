{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Main where

import           "base" Control.Concurrent                               (forkIO,
                                                                          threadDelay)
import           "base" Control.Monad                                    (forever,
                                                                          replicateM)
import           "base" Control.Monad.IO.Class                           (MonadIO,
                                                                          liftIO)
import           "base" Data.Dynamic                                     (Dynamic,
                                                                          fromDynamic,
                                                                          toDyn)
import           "base" Data.Functor                                     (void)
import           "base" Data.Functor.Identity                            (Identity)
import           "base" Data.Kind                                        (type (*),
                                                                          Constraint)
import           "base" Data.Proxy                                       (Proxy (..))
import           "base" Data.Typeable                                    (Typeable,
                                                                          cast,
                                                                          typeRep)
import           "base" Data.Void                                        (Void,
                                                                          absurd)
import           "base" GHC.Generics                                     (Generic)
import           "base" GHC.TypeLits                                     (KnownSymbol,
                                                                          Symbol,
                                                                          symbolVal)

import           "mtl" Control.Monad.Reader.Class                        (MonadReader,
                                                                          ask)
import           "mtl" Control.Monad.State.Class                         (MonadState,
                                                                          get,
                                                                          modify,
                                                                          put,
                                                                          state)
import           "mtl" Control.Monad.Writer.Class                        (MonadWriter,
                                                                          tell)

import           "transformers" Control.Monad.Trans.Class                (lift)
import           "transformers" Control.Monad.Trans.Reader               (ReaderT,
                                                                          runReaderT)
import           "transformers" Control.Monad.Trans.State                (StateT,
                                                                          runStateT)
import           "transformers" Control.Monad.Trans.Writer               (Writer,
                                                                          runWriter)

import           "type-fun" TypeFun.Constraint                           (AllSatisfy)
import           "type-fun" TypeFun.Data.List                            (Delete,
                                                                          Elem,
                                                                          SubList)

import           "binary" Data.Binary                                    (Binary,
                                                                          decode,
                                                                          encode)
import qualified "binary" Data.Binary                                    as Bin

import           "bytestring" Data.ByteString.Lazy                       (ByteString)

import           "network-transport" Network.Transport                   (Transport)

import qualified "network-transport-inmemory" Network.Transport.InMemory as InMemory
import           "network-transport-tcp" Network.Transport.TCP           (createTransport,
                                                                          defaultTCPParameters)

import qualified "rank1dynamic" Data.Rank1Dynamic                        as R1Dyn

import           "distributed-process" Control.Distributed.Process       (Process,
                                                                          ProcessId,
                                                                          expect,
                                                                          expectTimeout,
                                                                          getLocalNodeStats,
                                                                          getSelfPid,
                                                                          nodeStatsNode,
                                                                          processNodeId,
                                                                          say,
                                                                          send,
                                                                          spawn,
                                                                          spawnLocal)
import           "distributed-process" Control.Distributed.Process.Node  (initRemoteTable,
                                                                          newLocalNode,
                                                                          runProcess)
import           "distributed-static" Control.Distributed.Static         (Closure,
                                                                          RemoteTable,
                                                                          Static,
                                                                          closure,
                                                                          registerStatic,
                                                                          staticClosure,
                                                                          staticLabel)


newtype Union (f :: * -> *) (s :: [*]) = Union Dynamic

liftUnion :: (Typeable (f a), Typeable a, Elem a s) => f a -> Union f s
liftUnion = Union . toDyn
{-# INLINE liftUnion #-}

restrict :: Typeable (f a) => Union f s -> Either (Union f (Delete a s)) (f a)
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

typesExhausted :: a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}

class ExtractUnion c f s where
    extractWithIndex :: (forall a. (c (f a), Typeable a, Typeable (f a)) => Word -> f a -> b) -> Word -> Union f s -> b
    extract :: (forall a. (c (f a), Typeable a, Typeable (f a)) => f a -> b) -> Union f s -> b
    extract f = extractWithIndex @c @f @s (const f) 0
instance ExtractUnion c f '[] where
    extractWithIndex _ _ _ = typesExhausted
    {-# INLINE extractWithIndex #-}
instance (ExtractUnion c f (Delete a as) , c (f a), Typeable a, Typeable (f a)) => ExtractUnion c f (a ': as) where
    extractWithIndex f i u = case restrict u of
        Left sub           -> extractWithIndex @c @f @(Delete a as) f (succ i) sub
        Right (a :: (f a)) -> f i a
    {-# INLINE extractWithIndex #-}

indexUnion :: ExtractUnion Typeable f s => Union f s -> Word
indexUnion = extractWithIndex @Typeable const 0

liftExtract2 :: forall c f g s t d. (ExtractUnion c f s, ExtractUnion c g t)
             => (forall a b.
                    ( c (f a), Typeable (f a), Typeable a
                    , c (g b), Typeable (g b), Typeable b
                    ) => f a -> g b -> d
                )
            -> Union f s -> Union g t -> d
liftExtract2 f = extract @c (\ a -> extract @c (f a) )


class BinaryUnion f s where
    putUnion :: Word -> Union f s -> Bin.Put
    getUnion :: Word -> Bin.Get (Union f s)

instance BinaryUnion f '[] where
    putUnion = typesExhausted
    {-# INLINE putUnion #-}
    getUnion = typesExhausted
    {-# INLINE getUnion #-}

instance (BinaryUnion f (Delete a as), Binary (f a), Typeable (f a)) => BinaryUnion f (a ': as) where
    putUnion i u = case restrict u of
        Left sub -> putUnion (succ i) sub
        Right (a :: (f a)) -> do
            Bin.put i
            Bin.put a
    {-# INLINE putUnion #-}
    getUnion 0 = Union . toDyn <$> Bin.get @(f a)
    getUnion i = do
        (Union d) <- getUnion @f @(Delete a as) (pred i)
        pure $ Union d
    {-# INLINE getUnion #-}

instance BinaryUnion f s => Binary (Union f s) where
    put = putUnion 0
    get = Bin.get @Word >>= getUnion

instance ExtractUnion Show f s => Show (Union f s) where
    show = extract @Show $ \ u ->  "Union (" ++ show u ++ " :: " ++ show (typeRep [u]) ++ ")"

instance ExtractUnion Eq f s => Eq (Union f s) where
    u0 == u1 = extract @Eq (\ a -> Just a == extract @Eq cast u1) u0

instance (ExtractUnion Eq f s, ExtractUnion Ord f s, ExtractUnion Typeable f s) => Ord (Union f s) where
    compare u0 u1 = compare (indexUnion u0) (indexUnion u1)
                 <> liftExtract2 @Ord (\ a b -> Just a `compare` cast b) u0 u1

newtype ActorRef a = ActorRef ProcessId
  deriving (Eq, Ord, Generic)
instance Binary (ActorRef a)

instance Typeable a => Show (ActorRef a) where
    showsPrec d(ActorRef b) = showParen (d > 10)
                            $ showString (showType (Proxy @a))
                            . showString "@"
                            . shows b

data Send (a :: *) = Send
    { to  :: ActorRef a
    , msg :: Message a
    }
deriving instance (Eq (ActorRef a), Eq (Message a)) => Eq (Send a)
deriving instance (Show (ActorRef a), Show (Message a)) => Show (Send a)

newtype Create a = Create a
  deriving (Eq, Show)

data SystemMessage (as :: [*])
    = SysSend   (Union Send as)
    | SysCreate (Union Create as)
  deriving (Generic)
deriving instance (Eq (Union Send as), Eq (Union Create as)) => Eq (SystemMessage as)
deriving instance (Show (Union Send as), Show (Union Create as)) => Show (SystemMessage as)
instance (BinaryUnion Create as, BinaryUnion Send as) => Binary (SystemMessage as)

data Obit a = Death
    { deadRef      :: ActorRef a
    , deadState    :: a
    , causeOfDeath :: String
    }
  deriving (Eq, Ord, Show, Generic)
instance Binary a => Binary (Obit a)
data Signal a
    = Created
    | Obit (Union Obit (Creates a))
  deriving Generic

deriving instance (Eq (Union Obit (Creates a))) => Eq (Signal a)
deriving instance (Show (Union Obit (Creates a))) => Show (Signal a)
instance (Binary (Union Obit (Creates a))) => Binary (Signal a)

newtype MockActorAction sys a v
    = MockActorAction (ReaderT (ActorRef a) (StateT (Word, a) (Writer [SystemMessage sys])) v)
  deriving (Functor, Applicative, Monad, MonadReader (ActorRef a), MonadWriter [SystemMessage sys])

instance MonadState a (MockActorAction sys a) where
    state f = MockActorAction . state $ (\(id, a) -> fmap (id,) (f a))

runMockActorAction :: forall as a v. ActorActionConstraints as a => a -> MockActorAction as a v -> ((a , [SystemMessage as]), v)
runMockActorAction a (MockActorAction m)
    = let ((v, (_, a')), msgs) = runWriter (runStateT (runReaderT m (ActorRef . decode $ "root")) (1, a))
       in ((a', msgs), v)

class (MonadState a m, ActorActionConstraints sys a, CapabillitiesConstraints sys a m)
      => ActorAction (sys :: [*]) a (m :: * -> *)
      | m -> sys, m -> a where
    self :: m (ActorRef a)

    create :: (Actor b, b `Elem` Creates a, b `Elem` sys, ActorActionConstraints sys b)
           => b -> m (ActorRef b)

    (!) :: (Actor b, b `Elem` Sends a, b `Elem` sys, ActorActionConstraints sys b)
        => ActorRef b -> Message b -> m ()

type CapabillitiesConstraints sys a m = (m `ImplementsAll` Capabillities a, m `ImplementsAllCapabillities` sys)
type family ImplementsAll (a :: k) (c :: [k -> Constraint]) :: Constraint where
    ImplementsAll a '[] = ()
    ImplementsAll a (c ': cs) = (c a, ImplementsAll a cs)
type family ImplementsAllCapabillities (m :: * -> *) (sys :: [*]) :: Constraint where
    ImplementsAllCapabillities m '[] = ()
    ImplementsAllCapabillities m (a ': as) = (m `ImplementsAll` Capabillities a, ImplementsAllCapabillities m as)

class ( Typeable sys
      , Actor `AllSatisfy` sys
      , a `Elem` sys
      , Creates a `SubList` sys
      , Sends a `SubList` sys
      ) => ActorActionConstraints (sys :: [*]) a
instance ( Typeable sys
         , Actor `AllSatisfy` sys
         , a `Elem` sys
         , Creates a `SubList` sys
         , Sends a `SubList` sys
         ) => ActorActionConstraints (sys :: [*]) a


instance (ActorActionConstraints sys a, CapabillitiesConstraints sys a (MockActorAction sys a))
         => ActorAction sys a (MockActorAction sys a) where
    self = ask

    create :: forall b . (Actor b, b `Elem` Creates a, b `Elem` sys, ActorActionConstraints sys b)
           => b -> MockActorAction sys a (ActorRef b)
    create b = do
        tell [SysCreate (liftUnion (Create b))]
        nextId <- MockActorAction . state $ (\ (i, a) -> (i, (succ i, a)))
        pure (ActorRef (decode . encode $ nextId))

    ref ! msg = tell [SysSend (liftUnion (Send ref msg))]


newtype DistributedActorAction sys a v
    = DistributedActorAction
        { runDAA :: StateT a Process v
        }
  deriving (Functor, Applicative, Monad, MonadState a, MonadIO)

liftProcess :: Process v -> DistributedActorAction sys a v
liftProcess = DistributedActorAction . lift

instance (ActorActionConstraints sys a, CapabillitiesConstraints sys a (DistributedActorAction sys a))
         => ActorAction sys a (DistributedActorAction sys a) where
    self = ActorRef <$> liftProcess getSelfPid

    (!) :: forall b. (Actor b, Elem b (Sends a), Elem b sys, ActorActionConstraints sys b)
        => ActorRef b -> Message b -> DistributedActorAction sys a ()
    (!) ref = liftProcess . sendInternal ref . Right

    create b = liftProcess $ do
        nid <- processNodeId <$> getSelfPid
        pid <- spawn nid (runActorClosure (Proxy @sys) b)
        let newRef = ActorRef pid
        pure newRef

sendInternal :: Actor a => ActorRef a -> Either (Signal a) (Message a) -> Process ()
sendInternal (ActorRef pid) = send pid

showType :: Typeable a => proxy a -> String
showType p = "<<" ++ show (typeRep p) ++ ">>"

runActorLabel :: (Typeable sys, Typeable a) => proxy0 sys -> proxy1 a -> String
runActorLabel pSys pA = "$runActor" ++ showType pA ++ "in" ++ showType pSys

runActorStatic :: forall sys a. (Typeable sys, Typeable a) => Proxy sys -> Proxy a -> Static (ByteString -> Process ())
runActorStatic pSys pA = staticLabel $ runActorLabel pSys pA

runActorClosure:: forall (sys :: [*]) a. (Typeable sys, Actor a) => Proxy sys -> a -> Closure (Process ())
runActorClosure pSys = closure (runActorStatic pSys (Proxy @a)) . encode

runActor :: forall sys a. (Actor a, ActorAction sys a (DistributedActorAction sys a))
         => Proxy sys -> Proxy a -> ByteString -> Process ()
runActor pSys pA = void . runStateT (runDAA runActorDAA) . decode @a
  where
    runActorDAA :: DistributedActorAction sys a ()
    runActorDAA = do
        onSignal Created
        forever $ liftProcess expect >>= either onSignal behavior


class Monad m => Logging m where
    logStr :: String -> m ()

instance Logging (DistributedActorAction sys a) where
    logStr = liftProcess . say

class Monad m => Scheduling m where
    scheduleSendOnce :: forall b a sys.
                      ( Actor b
                      , b `Elem` Sends a
                      , b `Elem` sys
                      , ActorActionConstraints sys b
                      , ActorAction sys a m
                      )
                     => Int -> ActorRef b -> Message b -> m ()

instance ActorAction sys a (DistributedActorAction sys a) => Scheduling (DistributedActorAction sys a) where
    scheduleSendOnce d ref msg = void . liftProcess . spawnLocal $ do
        liftIO . threadDelay $ d * 1000
        sendInternal ref (Right msg)

class (as `SubList` sys) => RegisterBehaviors (sys :: [*]) (as :: [*]) where
    registerBehaviors :: Proxy as -> Proxy sys -> RemoteTable -> RemoteTable
instance RegisterBehaviors sys '[] where
    registerBehaviors _ _ = id
instance (RegisterBehaviors sys as, Actor a, ActorAction sys a (DistributedActorAction sys a)) => RegisterBehaviors sys (a ': as) where
    registerBehaviors _ pSys = let pA = Proxy @a
                                in registerStatic (runActorLabel pSys pA) (R1Dyn.toDynamic (runActor pSys pA))
                                 . registerBehaviors (Proxy @as) pSys


runActorSystem :: forall sys a.
                   ( Actor a
                   , ActorAction sys a (DistributedActorAction sys a)
                   , RegisterBehaviors sys sys
                   )
               => Proxy sys -> a -> Transport -> IO ()
runActorSystem pSys a t = do
    node <- newLocalNode t (registerBehaviors pSys pSys initRemoteTable)
    runProcess node (runActor pSys (Proxy @a) (encode a))


class (Binary a, Typeable a, Eq a, Show a) => RichData a
instance (Binary a, Typeable a, Eq a, Show a) => RichData a

class ( RichData a
      , RichData (Message a)
      , RichData (Union Obit (Creates a))
      , Actor `AllSatisfy` Sends a
      , Actor `AllSatisfy` Creates a
      ) => Actor a where
    type Message a
    type Message a = Void

    type Sends a :: [*]
    type Sends a = '[]

    type Creates a :: [*]
    type Creates a = '[]

    type Capabillities a :: [(* -> *) -> Constraint]
    type Capabillities a = '[]

    onSignal :: ActorAction sys a m => Signal a -> m ()
    onSignal _ = noop

    behavior :: ActorAction sys a m => Message a -> m ()
    default behavior :: (Message a ~ Void) => ActorAction sys a m => Message a -> m ()
    behavior = absurd


noop :: Applicative f => f ()
noop = pure ()


data ForkMsg
    = TakeFork (ActorRef Hakker)
    | PutFork  (ActorRef Hakker)
  deriving (Eq, Ord, Show, Generic)
instance Binary ForkMsg
data Fork
    = Available
    | TakenBy (ActorRef Hakker)
  deriving (Eq, Ord, Show, Generic)
instance Binary Fork
instance Actor Fork where
    type Sends Fork = '[Hakker]
    type Message Fork = ForkMsg
    behavior msg = get >>= \case
        Available -> case msg of
            TakeFork hakker -> do
                put (TakenBy hakker)
                self >>= (hakker !) . Taken
            _ -> noop
        TakenBy hakker -> case msg of
            TakeFork otherHakker -> self >>= (hakker !) . Busy
            PutFork otherHakker
                | hakker == otherHakker -> put Available
                | otherwise -> noop

data HakkerMsg
    = Busy  (ActorRef Fork)
    | Taken (ActorRef Fork)
    | Eat
    | Think
  deriving (Eq, Ord, Show, Generic)
instance Binary HakkerMsg
data Hakker = Hakker
    { name       :: String
    , left       :: ActorRef Fork
    , right      :: ActorRef Fork
    , hakkaState :: HakkerState
    }
  deriving (Eq, Ord, Show, Generic)
instance Binary Hakker
data HakkerState
    = Thinking
    | Hungry
    | WaitingFor (ActorRef Fork) (ActorRef Fork)
    | DeniedAFork
    | Eating
    | Recieve
    | StartThinking Word
  deriving (Eq, Ord, Show, Generic)
instance Binary HakkerState

instance Actor Hakker where
    type Sends Hakker = '[Fork, Hakker]
    type Message Hakker = HakkerMsg
    type Capabillities Hakker = '[Logging, Scheduling]
    behavior msg = get >>= \ (Hakker n l r s) -> realBehavior n l r s msg
      where
        become s = modify (\ s' -> s'{ hakkaState = s })
        startThinking d = do
            me <- self
            become Thinking
            scheduleSendOnce d me Eat
        realBehavior name left right = \case
            Thinking -> \case
                Eat -> do
                    become Hungry
                    self >>= (left !) . TakeFork
                    self >>= (right !) . TakeFork
            Hungry -> \case
                Taken fork
                    | fork == left  -> become $ WaitingFor right left
                    | fork == right -> become $ WaitingFor left right
                    | otherwise     -> error $ "I didn't ask for fork " ++ show fork
                Busy fork -> become DeniedAFork
            WaitingFor w o -> \case
                Taken w'
                    | w == w' -> do
                        logStr $ name ++ " has picked up " ++ show left ++ " and " ++ show right ++ " and starts to eat."
                        become Eating
                        self >>= (! Think)
                    | otherwise -> error $ "Not Waiting for fork " ++ show  w'
                Busy _ -> do
                    self >>= (o !) . PutFork
                    startThinking 1
            DeniedAFork -> \case
                Taken fork -> do
                    self >>= (fork !) . PutFork
                    startThinking 1
                Busy _ -> startThinking 1
            Eating -> \case
                Think -> do
                    self >>= (left !) . PutFork
                    self >>= (right !) . PutFork
                    logStr $ name ++ " puts down his forks and starts to think"
                    startThinking 20
            Recieve -> \case
                Think -> do
                    logStr $ name ++ " starts to think"
                    startThinking 20


data Dinner = Dinner
  deriving (Eq, Show, Ord, Generic)
instance Binary Dinner
instance Actor Dinner where
    type Sends Dinner = '[Hakker]
    type Creates Dinner = '[Fork, Hakker]
    type Capabillities Dinner = '[Logging]
    onSignal Created = do
        forks <- replicateM 5 . create $ Available
        let leftRight = zip forks . tail . cycle $ forks
        let hakkerList = zip ["Ghosh","Boner","Klang","Krasser","Manie"] leftRight
        let createHakker (name, (l, r)) = create $ Hakker name l r Recieve
        hakkers <- mapM createHakker hakkerList
        mapM_ (! Think) hakkers
        logStr "Created all hakkers"


newtype Ping = Ping (Maybe (ActorRef Pong))
  deriving (Eq, Show, Ord, Generic)
instance Binary Ping
instance Actor Ping where
    type Sends Ping = '[Pong]
    type Creates Ping = '[Pong]
    type Message Ping = Int
    type Capabillities Ping = '[Logging, Scheduling]
    onSignal Created = do
        ref <- create . Pong =<< self
        put . Ping . Just $ ref
        ref ! 0
    behavior i = do
        (Ping (Just ref)) <- get
        logStr $ "ping " ++ show i
        scheduleSendOnce 1000 ref $ succ i

newtype Pong = Pong (ActorRef Ping)
  deriving (Eq, Show, Ord, Generic)
instance Binary Pong
instance Actor Pong where
    type Sends Pong = '[Ping]
    type Message Pong = Int
    type Capabillities Pong = '[Logging, Scheduling]
    behavior i = do
        (Pong ref) <- get
        logStr $ "pong " ++ show i
        scheduleSendOnce 1000 ref $ succ i

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" ("127.0.0.1",) defaultTCPParameters
    runActorSystem (Proxy @'[Ping, Pong]) (Ping Nothing) t
