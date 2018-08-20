{-# LANGUAGE Trustworthy #-} -- Generalized newtype deriving for MockActorContext
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Dakka.MockActorContext where

import           "base"         Data.Maybe    ( fromMaybe )
import           "base"         Data.Typeable ( typeRep )

import           "containers"   Data.Tree ( Tree(..) )

import           "bytestring"   Data.ByteString.Lazy.Char8 ( pack )

import           "transformers" Control.Monad.Trans.State.Lazy  ( StateT, State, runStateT, runState, evalState )
import           "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import           "mtl"          Control.Monad.Reader       ( ReaderT, ask, MonadReader, runReaderT )
import           "mtl"          Control.Monad.State.Class  ( MonadState( get, put ), gets )
import           "mtl"          Control.Monad.Writer.Class ( MonadWriter( tell ) )

import                          Dakka.Actor       ( ActorContext( self, create' , (!) ), Actor( Message, startState ) )
import                          Dakka.Actor.Base  ( ActorRef( ActorRef ) )
import                          Dakka.Constraints ( (=~=) )
import                          Dakka.HMap        ( HMap ) 
import qualified                Dakka.HMap        as HMap


-------------------
-- SystemMessage --
-------------------

data Creates where
    Creates :: Actor a => ActorRef a -> Creates
data Send where
    Send :: Actor a => ActorRef a -> Message a -> Send

-- | Encapsulates an interaction of a behavior with the context
type SystemMessage = Either Creates Send

instance Show Creates where
    showsPrec d (Creates p)     = showParen (d > 10) 
                                $ showString "Creates <<"
                                . shows (typeRep p)
                                . showString ">>"
instance Show Send where                            
    showsPrec d (Send to' msg') = showParen (d > 10)
                                $ showString "Send {to = "
                                . showsPrec 11 to'
                                . showString ", msg = "
                                . shows msg'
                                . showString "}"

instance Eq Creates where
    (Creates a)    == (Creates b)    = a =~= b
instance Eq Send where
    (Send refa am) == (Send refb bm) = refa =~= refb && am =~= bm 

--------------
-- CtxState --
--------------

data CtxState = CtxState
    { nextId :: Word
    , states :: HMap ActorRef
    }
  deriving 
    ( Show
    , Eq
    )

ctxEmpty :: CtxState
ctxEmpty = CtxState 0 HMap.hEmpty

ctxRegisterActor :: forall a m. (MonadState CtxState m, Actor a) => a -> m (ActorRef a)
ctxRegisterActor a = do
    (CtxState i m) <- get
    let ref = ActorRef . pack . show $ i
    let m' = HMap.hInsert ref a m
    put $ CtxState (succ i) m'
    return ref

ctxCreateActor :: forall a m. (MonadState CtxState m, Actor a) => m (ActorRef a)
ctxCreateActor = ctxRegisterActor startState

ctxGetState :: (MonadState CtxState m, Actor a) => ActorRef a -> m (Maybe a)
ctxGetState ref = gets (HMap.hLookup ref . states) 

ctxLookup :: Actor a => ActorRef a -> CtxState -> a
ctxLookup ref (CtxState _ m) = startState `fromMaybe` HMap.hLookup ref m

----------------------
-- MockActorContext --
----------------------

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext a v = MockActorContext
    ( ReaderT (ActorRef a) 
        ( StateT CtxState 
            (Writer [SystemMessage])
        ) v
    )
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [SystemMessage]
    , MonadReader (ActorRef a)
    )

instance Actor a => MonadState a (MockActorContext a) where
    get = do
        ref <- ask
        MockActorContext . gets $ ctxLookup ref
    put a = do
        ref <- ask
        MockActorContext $ do
            CtxState i m <- get
            let m' = HMap.hInsert ref a m 
            put $ CtxState i m'

instance Actor a => ActorContext a (MockActorContext a) where

    self = ask

    create' _ = do 
      ref <- MockActorContext ctxCreateActor
      tell [Left $ Creates ref]
      pure ref

    p ! m = tell [Right $ Send p m]


type MockResult a = (a, [SystemMessage])

runMockInternal :: forall a v. Actor a => MockActorContext a v -> ActorRef a -> CtxState -> ((v, CtxState), [SystemMessage])
runMockInternal (MockActorContext ctx) ref = runWriter . runStateT (runReaderT ctx ref)

runMockAllInternal :: SystemMessage -> CtxState -> Tree (CtxState, SystemMessage)
runMockAllInternal = evalState . go 
  where
    go :: SystemMessage -> State CtxState (Tree (CtxState, SystemMessage))
    go = undefined
    {-
    go (Left (Creates ref)) = do 
        a <- gets $ ctxLookup ref
        ((_, ctx), msgs) <- gets $ runMockInternal (behavior (Left Created)) ref  
        put ctx
        Node (ctx, msg) <$> traverse go msgs
    go (Right (Send ref payload)) = undefined
    -}


runMock :: forall a v. Actor a => MockActorContext a v -> a -> (v, MockResult a)
runMock ctx a = swapResult $ runMockInternal ctx ref initalCtx 
  where
    (ref, initalCtx) = ctxRegisterActor a `runState` ctxEmpty
    swapResult ((res, ctx'), msgs) = (res, (ctxLookup ref ctx', msgs))

runMock' :: forall a v. Actor a => MockActorContext a v -> (v, MockResult a) 
runMock' ctx = runMock ctx startState


execMock :: forall a v. Actor a => MockActorContext a v -> a -> MockResult a
execMock ctx a = snd $ runMock ctx a

execMock' :: forall a v. Actor a => MockActorContext a v -> MockResult a 
execMock' ctx = snd $ runMock' ctx


evalMock :: forall a v. Actor a => MockActorContext a v -> a -> v
evalMock ctx a = fst $ runMock ctx a

evalMock' :: forall a v. Actor a => MockActorContext a v -> v
evalMock' ctx = fst $ runMock' ctx

