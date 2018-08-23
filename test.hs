{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import           "base" Control.Arrow                           (second, (&&&))
import           "base" Control.Monad                           (join)
import           "base" Data.Functor                            (void)
import           "base" Data.Maybe                              (fromJust)
import           "base" Data.Monoid                             (Last (..))
import           "base" Data.Proxy                              (Proxy (..))
import           "base" Data.Typeable                           (cast, typeRep)
import           "base" GHC.Generics                            (Generic)
import           "base" Prelude                                 hiding (log)
import           "base" Type.Reflection                         (Typeable)
import           "base" Unsafe.Coerce                           (unsafeCoerce)

import           "containers" Data.Map                          (Map)
import qualified "containers" Data.Map                          as Map

import           "lens" Control.Lens                            (Lens',
                                                                 Traversal',
                                                                 lens,
                                                                 makeLenses,
                                                                 preuse,
                                                                 traverse, use,
                                                                 view, ( #%%= ),
                                                                 (&), (.=),
                                                                 (.~), (^.),
                                                                 _Just)

import           "mtl" Control.Monad.Reader                     (MonadReader,
                                                                 ReaderT, ask,
                                                                 runReaderT)
import           "mtl" Control.Monad.State.Class                (MonadState,
                                                                 get, gets, put,
                                                                 state)
import           "transformers" Control.Monad.Trans.State.Lazy  (StateT,
                                                                 runState,
                                                                 runStateT)
import           "transformers" Control.Monad.Trans.Writer.Lazy (Writer,
                                                                 runWriter)
import           "mtl" Control.Monad.Writer.Class               (MonadWriter,
                                                                 tell)

import           "freer-simple" Control.Monad.Freer             (Eff,
                                                                 LastMember,
                                                                 Member,
                                                                 Members,
                                                                 interpretM,
                                                                 run)
import qualified "freer-simple" Control.Monad.Freer             as Freer
import           "freer-simple" Control.Monad.Freer.Internal    (Arr,
                                                                 handleRelay)
import           "freer-simple" Control.Monad.Freer.State       (State, modify)

import           "binary" Data.Binary                           (encode)

import           "bytestring" Data.ByteString.Lazy              (ByteString)

{-# ANN module ("HLint: ignore Redundant compare" :: String) #-}


-- ActorRef --

newtype ActorRef a = ActorRef { actorId :: ByteString }
  deriving (Eq, Ord, Generic, Functor)

instance Typeable a => Show (ActorRef a) where
    showsPrec d ref
        = showParen (d > 10)
        $ showString "ActorRef <<"
        . shows (typeRep (Proxy @a))
        . showString ">>@"
        . shows (actorId ref)

-- Actor actions --

data Create (a :: *) r where
    Create :: Create a (ActorRef a)
create :: forall a effs. (Actor a, Member (Create a) effs) => Eff effs (ActorRef a)
create = Freer.send Create


data Send r where
    Send :: Actor a => ActorRef a -> a -> Send ()

send :: forall a effs. (Actor a, Member Send effs) => ActorRef a -> a -> Eff effs ()
send ref = Freer.send . Send ref

(!) :: forall a effs. (Actor a, Member Send effs) => ActorRef a -> a -> Eff effs ()
(!) = send

(<!>) :: forall a effs m. (Actor a, Member Send effs) => Eff effs (ActorRef a) -> a -> Eff effs ()
a <!> m = a >>= (! m)

instance Show (Send r) where
    showsPrec d (Send ref msg)
        = showParen (d > 10)
        $ showString "Send "
        . showsPrec 11 ref
        . showString " "
        . showsPrec 11 msg


data Log r where
    Log :: String -> Log ()
log :: forall a effs. (Actor a, Member Log effs) => String -> Eff effs ()
log = Freer.send . Log

data Self a r where
    Self :: Self a (ActorRef a)
self :: forall a effs. (Actor a, Member (Self a) effs) => Eff effs (ActorRef a)
self = Freer.send Self

-- HList for states --

type family AllStates (effs :: [* -> *]) :: [*] where
    AllStates '[]               = '[]
    AllStates (State a ': effs) = a ': AllStates effs
    AllStates (eff     ': effs) = AllStates effs


data family HList (f :: * -> *) (l :: [*])

data instance HList f '[] = HNil
data instance HList f (x ': xs) = f x `HCons` HList f xs

(<:>) :: Applicative f => a -> HList f as -> HList f (a ': as)
a <:> l = pure a `HCons` l

deriving instance Eq (HList f '[])
deriving instance (Eq (f x), Eq (HList f xs)) => Eq (HList f (x ': xs))

deriving instance Ord (HList f '[])
deriving instance (Ord (f x), Ord (HList f xs)) => Ord (HList f (x ': xs))

instance Show (HList f '[]) where
    show _ = "H[]"

instance {-# OVERLAPPABLE #-} (Show (f e), Show (HList f l)) => Show (HList f (e ': l)) where
    show (HCons x l) = let 'H':'[':s = show l
                       in "H[" ++ show x ++
                                  (if s == "]" then s else "," ++ s)
instance {-# OVERLAPPING #-} (Show e, Show (HList Last l)) => Show (HList Last (e ': l)) where
    show (HCons (Last x) l) = let 'H':'[':s = show l
                              in "H[" ++ maybe "_" show x ++
                                  (if s == "]" then s else "," ++ s)

class e ∈ (l :: [*]) where
    hAt :: Lens' (HList f l) (f e)
    hAt = lens getElem setElem
    getElem :: HList f l -> f e
    setElem :: HList f l -> f e -> HList f l
instance e ∈ (e ': es) where
    getElem (e `HCons` _)    = e
    setElem (_ `HCons` es) e = e `HCons` es
instance {-# OVERLAPPABLE #-} (e ∈ es) => e ∈ (e' ': es) where
    getElem (_ `HCons` es)   = getElem es
    setElem (e `HCons` es) a = e `HCons` setElem es a

type ActorState a = HList Last (AllStates (Can a))

-- mempty removed from Monoid --

class HasDefault a where
    empty :: a
instance HasDefault (HList f '[]) where
    empty = HNil
instance (HasDefault a, HasDefault (HList Last as))
      => HasDefault (HList Last (a ': as)) where
    empty = pure empty `HCons` empty

-- Actor --

data Signal where
    Created :: Signal
    Obit    :: Actor a => ActorRef a -> ActorState a -> Signal
  deriving Typeable

instance Show Signal where
    showsPrec _ Created = showString "Created"
    showsPrec d (Obit ref s)
        = showParen (d > 10)
        $ showString "Obit "
        . showsPrec 11 ref
        . showString " "
        . showsPrec 11 s


type Msg a = Either Signal a


class (Typeable a, Show a, Typeable (ActorState a), Show (ActorState a), Eq (ActorState a), HasDefault (ActorState a))
      => Actor a where
    type Can a :: [* -> *]
    type Can a = '[]
    behavior :: Members (Can a) effs => Msg a -> Eff effs ()
    behavior _ = pure ()

-- MockActorSystem --

data SystemMessage where
    SysCreate :: Actor a => ActorRef a -> SystemMessage
    SysSend   :: Actor a => ActorRef a -> a -> SystemMessage

instance Show SystemMessage where
    showsPrec d (SysCreate ref)
        = showParen (d > 10)
        $ showString "SysCreate "
        . showsPrec 11 ref
    showsPrec d (SysSend ref msg)
        = showParen (d > 10)
        $ showString "SysSend "
        . showsPrec 11 ref
        . showString " "
        . showsPrec 11 msg


data SKey where
    SKey :: Actor a => ActorRef a -> SKey
instance Eq SKey where
    k0 == k1 = compare k0 k1 == EQ
instance Ord SKey where
    SKey r0 `compare` SKey r1 = typeRep r0 `compare` typeRep r1 <> Just r0 `compare` cast r1
instance Show SKey where showsPrec d (SKey r) = showsPrec d r
data SElem where
    SElem :: Actor a => Proxy a -> ActorState a -> SElem
instance Show SElem where showsPrec d (SElem _ s) = showsPrec d s
instance Eq SElem where SElem _ a == SElem _ b = Just a == cast b

data MockSystemState = MockSystemState
    { _nextId :: Word
    , _states :: Map SKey SElem
    }
  deriving Eq
makeLenses ''MockSystemState

instance Show MockSystemState where
    showsPrec d (MockSystemState i s)
        = showParen (d > 10)
        $ showString "MSS "
        . shows i
        . showString " "
        . showString m
      where
        ('f':'r':'o':'m':'L':'i':'s':'t':' ':m) = show s

nextRef :: forall a m. MonadState MockSystemState m => m (ActorRef a)
nextRef = nextId #%%= (ActorRef . encode &&& succ)

instance HasDefault MockSystemState where
    empty = MockSystemState 1 Map.empty

sAt :: forall a. Actor a => ActorRef a -> Lens' MockSystemState (Maybe (ActorState a))
sAt k = states . lens hLookup hInsert
  where
    hInsert m Nothing  = Map.delete (SKey k) m
    hInsert m (Just v) = Map.insert (SKey k) (SElem (Proxy @a) v) m
    hLookup m = (\ (SElem _ e) -> unsafeCoerce e) <$> Map.lookup (SKey k) m
ixSAt :: Actor a => ActorRef a -> Traversal' MockSystemState (ActorState a)
ixSAt k = sAt k . traverse

mockActorState :: forall a e f. (Actor a, Applicative f, e ∈ AllStates (Can a))
               => ActorRef a
               -> Traversal' MockSystemState (Last e)
mockActorState ref = sAt ref . _Just . hAt


newtype MockActorSystem a r = MockActorSystem
    { unMAS :: ReaderT (ActorRef a) (StateT MockSystemState (Writer [SystemMessage])) r }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ActorRef a)
    , MonadWriter [SystemMessage]
    , MonadState MockSystemState
    )

-- run MockActorSystem --

class Monad m => RunFreer m eff where
    runEff :: Eff (m ': eff ': effs) a -> Eff (m ': effs) a

instance MonadReader (ActorRef a) m => RunFreer m (Self a ': effs) where
    runEff = runEff . Freer.interpretM (\ Self -> ask)

instance (LastMember m effs, MonadWriter [SystemMessage] m, RunFreer m effs)
      => RunFreer m (Send ': effs) where
    runEff = runEff . Freer.interpretM (\ (Send ref msg) -> tell [SysSend ref msg])

instance (LastMember m effs, MonadWriter [SystemMessage] m, MonadState MockSystemState m, RunFreer m effs, Actor a)
      => RunFreer m (Create a ': effs) where
    runEff = runEff . Freer.interpretM createM
      where
        createM :: Create a v -> m v
        createM Create = do
            ref <- nextRef @a
            tell [SysCreate ref]
            pure ref

instance (LastMember m effs, MonadReader (ActorRef a) m, MonadState MockSystemState m, Actor a, e ∈ AllStates (Can a))
      => RunFreer m (State s ': effs)


runMockActorSystem :: forall a r. (Actor a, RunFreer (MockActorSystem a) (Can a))
                   => ActorAction a r
                   -> ActorRef a
                   -> MockSystemState
                   -> (([SystemMessage], MockSystemState), r)
runMockActorSystem (ActorAction a) ref = swapRes . runWriter . runStateT (runReaderT m ref)
  where
    swapRes ((r, s'), msgs) = ((msgs, s'), r)
    m = unMAS $ runEff @(MockActorSystem a) a


-- Test Actors --

instance HasDefault [a] where empty = []

data A0 = A0 deriving (Eq, Show)
instance Actor A0 where
    type Can A0 = '[State [String]]
    behavior = modify . (:) . show

data AState = AState deriving (Eq, Show)
instance Actor AState where
    type Can AState = '[State String, Self AState, Send, Create A0]
    behavior (Left Created) = do
        modify (++ " World")
        self <!> AState
        void $ create @A0
        modify (++ "!")

-- Main --

{-
main :: IO ()
main = do
    let s = flip runState empty $ do
        ref <- nextRef @AState
        sAt ref .= pure ("hello" <:> HNil)
        return ref
    let ((msgs, s'), ()) = uncurry (runMockActorSystem (behavior @AState (Left Created))) s
    print msgs
    print $ snd s
    print s'
-}
main = pure ()
