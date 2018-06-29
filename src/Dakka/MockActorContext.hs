{-# LANGUAGE Trustworthy #-} -- Generalized newtype deriving for MockActorContext
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import "base" Data.Proxy ( Proxy )
import "base" Data.Typeable ( typeRep )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, runStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState( state ) )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Actor


-------------------
-- SystemMessage --
-------------------

-- | Encapsulates an interaction of a behavior with the context
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall a. Actor a => Send
        { to  :: ActorRef a 
        , msg :: Message a
        }

instance Show SystemMessage where
    showsPrec d (Create p)      = showParen (d > 10) 
                                $ showString "Create <<"
                                . shows (typeRep p)
                                . showString ">>"
    showsPrec d (Send to' msg') = showParen (d > 10)
                                $ showString "Send {to = "
                                . showsPrec 11 to'
                                . showString ", msg = "
                                . shows msg'
                                . showString "}"

instance Eq SystemMessage where
    (Create a)     == (Create b)     = a =~= b
    (Send refa am) == (Send refb bm) = refa =~= refb && am =~= bm 
    _              == _              = False

----------------------
-- MockActorContext --
----------------------

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext a v = MockActorContext
    (ReaderT (ActorRef a) (StateT a (Writer [SystemMessage])) v)
  deriving (Functor, Applicative, Monad, MonadWriter [SystemMessage], MonadReader (ActorRef a))

instance MonadState a (MockActorContext a) where
    state = MockActorContext . state

instance Actor a => ActorContext a (MockActorContext a) where

    self = ask

    create' a = do
        tell [Create a]
        return $ ActorRef mempty

    p ! m = tell [Send p m]


type MockResult a = (a, [SystemMessage])

runMock :: forall a v. ActorRef a -> MockActorContext a v -> a -> (v, MockResult a)
runMock ar (MockActorContext ctx) a = swapResult $ runWriter $ runStateT (runReaderT ctx ar) a
  where swapResult ((res, a'), msgs) = (res, (a', msgs))

runMock' :: forall a v. Actor a => ActorRef a -> MockActorContext a v -> (v, MockResult a) 
runMock' ar ctx = runMock ar ctx startState

runMockRoot :: forall a v. MockActorContext a v -> a -> (v, MockResult a)
runMockRoot = runMock $ ActorRef mempty 

runMockRoot' :: forall a v. Actor a => MockActorContext a v -> (v, MockResult a)
runMockRoot' ctx = runMockRoot ctx startState


execMock :: forall a v. ActorRef a -> MockActorContext a v -> a -> MockResult a
execMock ar ctx a = snd $ runMock ar ctx a

execMock' :: forall a v. Actor a => ActorRef a -> MockActorContext a v -> MockResult a 
execMock' ar ctx = snd $ runMock' ar ctx

execMockRoot :: forall a v. MockActorContext a v -> a -> MockResult a 
execMockRoot ctx a = snd $ runMockRoot ctx a

execMockRoot' :: forall a v. Actor a => MockActorContext a v -> MockResult a
execMockRoot' ctx = snd $ runMockRoot' ctx 


evalMock :: forall a v. ActorRef a -> MockActorContext a v -> a -> v
evalMock ar ctx a = fst $ runMock ar ctx a

evalMock' :: forall a v. Actor a => ActorRef a -> MockActorContext a v -> v
evalMock' ar ctx = fst $ runMock' ar ctx

evalMockRoot :: forall a v. MockActorContext a v -> a -> v 
evalMockRoot ctx a = fst $ runMockRoot ctx a

evalMockRoot' :: forall a v. Actor a => MockActorContext a v -> v 
evalMockRoot' ctx = fst $ runMockRoot' ctx 

