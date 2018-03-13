{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , TypeApplications
           , DefaultSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
           , DataKinds
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , DeriveDataTypeable
           , PackageImports
           , TypeOperators
           , ConstraintKinds
           , PolyKinds
           , RankNTypes
           , UndecidableInstances
           , UndecidableSuperClasses 
#-}
module Dakka.MockActorContext where

import "base" Data.Proxy ( Proxy )
import "base" Data.Typeable ( typeRep )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState, modify )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Actor
import Dakka.Type.Path
import Dakka.Type.Tree
import Dakka.Constraints


-- | Encapsulates an interaction of a behavior with the context
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall p. Actor (Tip p) => Send
        { to  :: ActorRef p
        , msg :: Message (Tip p)
        }

instance Show SystemMessage where
    showsPrec i (Create p)    str = "Create " ++ show (typeRep p) ++ str
    showsPrec i (Send to msg) str = "Send {to = " ++ show to ++ ", msg = " ++ show msg ++ "}" ++ str

instance Eq SystemMessage where
    (Create a)   == (Create b)   = a =~= b
    (Send at am) == (Send bt bm) = demotePath at == demotePath bt && am =~= bm

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext t p a v = MockActorContext
    (ReaderT (ActorRef p) (StateT a (Writer [SystemMessage])) v)
  deriving (Functor, Applicative, Monad, MonadState a, MonadWriter [SystemMessage], MonadReader (ActorRef p))

instance (Actor a, a ~ Select p t, a ~ Tip p) => ActorContext t p a (MockActorContext t p a) where

    self = ask

    create' a = do
        tell [Create a]
        self <$/> a

    p ! m = tell [Send p m]

-- | Execute a 'Behavior' in a 'MockActorContext'.
execMock :: forall t a p b. (a ~ Select p t) => ActorRef p -> MockActorContext t p a b -> a -> (a, [SystemMessage])
execMock ar (MockActorContext ctx) = runWriter . execStateT (runReaderT ctx ar)


execMock' :: forall t a p b. (a ~ Select p t, Actor a) => ActorRef p -> MockActorContext t p a b -> (a, [SystemMessage])
execMock' ar ctx = execMock ar ctx startState

