{-# LANGUAGE Trustworthy #-} -- Generalized newtype deriving for MockActorContext
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Dakka.MockActorContext where

import "base" Data.Proxy ( Proxy )
import "base" Data.Typeable ( Typeable, typeRep, cast )
import "base" Data.Functor.Classes ( liftEq )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, runStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState( state ) )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Actor
import Dakka.Path


-------------------
-- SystemMessage --
-------------------

-- | Encapsulates an interaction of a behavior with the context
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall p m. 
        ( ActorRefConstraints p
        , Typeable m
        , Actor (Tip p)
        , Actor (PRoot p)
        ) => Send
            { to  :: ActorPath p
            , msg :: Message (Tip p) m
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
                                . showsMsg 11 msg'
                                . showString "}"

instance Eq SystemMessage where
    (Create a)               == (Create b)               = a =~= b
    (Send (ActorPath at) am) == (Send (ActorPath bt) bm) = demotePath at == demotePath bt 
                                                         && liftEq eqMsg (Just am) (cast bm)
    _                        == _                        = False

---------------
-- ActorPath --
---------------

newtype ActorPath p = ActorPath
    { path :: HPathT p () }
  deriving (Eq, Typeable)

instance ActorRef ActorPath
instance (p `AllSegmentsImplement` Typeable) => Show (ActorPath p) where
    showsPrec d (ActorPath p) = showParen (d > 10)
                              $ showString "ActorPath "
                              . shows p

----------------------
-- MockActorContext --
----------------------

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext p v = MockActorContext
    (ReaderT (ActorPath p) (StateT (Tip p) (Writer [SystemMessage])) v)
  deriving (Functor, Applicative, Monad, MonadWriter [SystemMessage], MonadReader (ActorPath p))

instance (a ~ Tip p) => MonadState a (MockActorContext p) where
    state = MockActorContext . state

instance ( ActorRefConstraints p
         , MonadState (Tip p) (MockActorContext p)
         ) => ActorContext p (MockActorContext p) where
    type CtxRef  (MockActorContext p) = ActorPath
    type CtxPath (MockActorContext p) = p

    self = ask

    create' a = do
        tell [Create a]
        ActorPath . (</> a) . path <$> self

    p ! m = tell [Send p m]


type MockResult a = (a, [SystemMessage])

runMock :: forall p b. ActorPath p -> MockActorContext p b -> Tip p -> (b, MockResult (Tip p))
runMock ar (MockActorContext ctx) a = swapResult $ runWriter $ runStateT (runReaderT ctx ar) a
  where swapResult ((res, a'), msgs) = (res, (a', msgs))

runMock' :: forall p b. Actor (Tip p) => ActorPath p -> MockActorContext p b -> (b, MockResult (Tip p))
runMock' ar ctx = runMock ar ctx startState

runMockRoot :: forall a b. MockActorContext ('Root a) b -> a -> (b, MockResult a)
runMockRoot = runMock $ ActorPath (root @a)

runMockRoot' :: forall a b. Actor a => MockActorContext ('Root a) b -> (b, MockResult a)
runMockRoot' ctx = runMockRoot ctx startState


execMock :: forall p b. ActorPath p -> MockActorContext p b -> Tip p -> MockResult (Tip p)
execMock ar ctx a = snd $ runMock ar ctx a

execMock' :: forall p b. Actor (Tip p) => ActorPath p -> MockActorContext p b -> MockResult (Tip p)
execMock' ar ctx = snd $ runMock' ar ctx

execMockRoot :: forall a b. MockActorContext ('Root a) b -> a -> MockResult a 
execMockRoot ctx a = snd $ runMockRoot ctx a

execMockRoot' :: forall a b. Actor a => MockActorContext ('Root a) b -> MockResult a
execMockRoot' ctx = snd $ runMockRoot' ctx 


evalMock :: forall p b. ActorPath p -> MockActorContext p b -> Tip p -> b
evalMock ar ctx a = fst $ runMock ar ctx a

evalMock' :: forall p b. Actor (Tip p) => ActorPath p -> MockActorContext p b -> b
evalMock' ar ctx = fst $ runMock' ar ctx

evalMockRoot :: forall a b. MockActorContext ('Root a) b -> a -> b 
evalMockRoot ctx a = fst $ runMockRoot ctx a

evalMockRoot' :: forall a b. Actor a => MockActorContext ('Root a) b -> b 
evalMockRoot' ctx = fst $ runMockRoot' ctx 

