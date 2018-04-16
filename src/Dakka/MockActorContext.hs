{-# LANGUAGE Trustworthy #-} -- Generalized newtype deriving for MockActorContext
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Dakka.MockActorContext where

import "base" Data.Proxy ( Proxy )
import "base" Data.Typeable ( Typeable, typeRep, cast )
import "base" Data.Functor.Classes ( liftEq )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState( state ) )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Actor
import Dakka.Path
import Dakka.Constraints


-- | Encapsulates an interaction of a behavior with the context
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall p. 
        ( ActorRefConstraints p
        , Actor (Tip p)
        , Actor (PRoot p)
        ) => Send
            { to  :: ActorPath p
            , msg :: Message (Tip p) ActorPath p
            }

instance Show SystemMessage where
    showsPrec d (Create p)      = showParen (d > 10) 
                                $ showString "Create "
                                . shows (typeRep p)
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

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext p v = MockActorContext
    (ReaderT (ActorPath p) (StateT (Tip p) (Writer [SystemMessage])) v)
  deriving (Functor, Applicative, Monad, MonadWriter [SystemMessage], MonadReader (ActorPath p))

instance MonadState a (MockActorContext ('Root a)) where
    state = MockActorContext . state
instance MonadState a (MockActorContext (as ':/ a)) where
    state = MockActorContext . state

newtype ActorPath p = ActorPath { path :: HPathT p ActorId }
    deriving (Eq, Show, Typeable)

instance ActorRef ActorPath where

instance ( ActorRefConstraints p
         , MonadState (Tip p) (MockActorContext p)
         ) => ActorContext p (MockActorContext p) where
    type Ref (MockActorContext p) = ActorPath
    self = ask

    create' a = do
        tell [Create a]
        (ActorPath p) <- self
        pure $ ActorPath $ p </> a

    p ! m = tell [Send p m]

-- | Execute a 'Behavior' in a 'MockActorContext'.
execMock :: forall p b. ActorPath p -> MockActorContext p b -> Tip p -> (Tip p, [SystemMessage])
execMock ar (MockActorContext ctx) = runWriter . execStateT (runReaderT ctx ar)


execMock' :: forall p b. Actor (Tip p) => ActorPath p -> MockActorContext p b -> (Tip p, [SystemMessage])
execMock' ar ctx = execMock ar ctx startState

