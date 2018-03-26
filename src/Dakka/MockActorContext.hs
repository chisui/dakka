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
import "base" Data.Typeable ( typeRep, TypeRep )

import "transformers" Control.Monad.Trans.State.Lazy ( StateT, execStateT )
import "transformers" Control.Monad.Trans.Writer.Lazy ( Writer, runWriter )

import "mtl" Control.Monad.Reader ( ReaderT, ask, MonadReader, runReaderT )
import "mtl" Control.Monad.State.Class ( MonadState, modify )
import "mtl" Control.Monad.Writer.Class ( MonadWriter( tell ) )

import Dakka.Convert
import Dakka.Actor
import Dakka.Type.Path
import Dakka.Type.Tree
import Dakka.Constraints


-- | Encapsulates an interaction of a behavior with the context
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall p. (Actor (Tip p)) => Send
        { to  :: ActorRef p
        , msg :: Message (Tip p)
        }

instance Show SystemMessage where
    showsPrec i (Create p)    str = "Create " ++ show (typeRep p) ++ str
    showsPrec i (Send to msg) str = "Send {to = " ++ show to ++ ", msg = " ++ show msg ++ "}" ++ str

instance Eq SystemMessage where
    (Create a)   == (Create b)   = a =~= b
    (Send at am) == (Send bt bm) = demotePath at == demotePath bt && am =~= bm
      where
        demotePath :: ActorRef p -> Path (Word, TypeRep)
        demotePath = convert

-- | An ActorContext that simply collects all state transitions, sent messages and creation intents.
newtype MockActorContext p v = MockActorContext
    (ReaderT (ActorRef p) (StateT (Tip p) (Writer [SystemMessage])) v)
  deriving (Functor, Applicative, Monad, MonadState (Tip p), MonadWriter [SystemMessage], MonadReader (ActorRef p))

instance ( ConsistentActorPath p
         , MonadState (Tip p) (MockActorContext p)
         , Actor (Tip p)
         ) => ActorContext p (MockActorContext p) where

    self = ask

    create' a = do
        tell [Create a]
        self <$/> a

    p ! m = tell [Send p m]

-- | Execute a 'Behavior' in a 'MockActorContext'.
execMock :: forall p b. ActorRef p -> MockActorContext p b -> Tip p -> (Tip p, [SystemMessage])
execMock ar (MockActorContext ctx) = runWriter . execStateT (runReaderT ctx ar)


execMock' :: forall p b. Actor (Tip p) => ActorRef p -> MockActorContext p b -> (Tip p, [SystemMessage])
execMock' ar ctx = execMock ar ctx startState

