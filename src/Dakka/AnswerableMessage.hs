{-# LANGUAGE ExistentialQuantification
           , TypeFamilies
           , KindSignatures
           , PackageImports
           , StandaloneDeriving
           , FlexibleContexts
           , TypeOperators
           , DataKinds
#-}
module Dakka.AnswerableMessage where

import "base" Data.Typeable ( Typeable, cast, TypeRep )
import "base" Data.Functor.Classes ( Eq1(..), Show1(..) )

import Dakka.Actor
import Dakka.Type.Path
import Dakka.Type.Tree
import Dakka.Convert


data AnswerableMessage r a = forall (p :: Path *).
    ( ConsistentActorPath p
    , a ~ PRoot p
    , Actor (Tip p)
    , Convertible (r (PRoot p)) ((Message (Tip p)) (PRoot p))
    ) => AnswerableMessage
        { askerRef :: ActorRef p
        }

deriving instance (Typeable a, Typeable r) => Typeable (AnswerableMessage a r)

instance Eq1 (AnswerableMessage a) where
    liftEq _ (AnswerableMessage a) (AnswerableMessage b) = demotePath a == demotePath b
      where
        demotePath :: ActorRef p -> Path (Word, TypeRep)
        demotePath = convert

instance Eq (AnswerableMessage a r) where
    (==) = liftEq undefined

instance Show1 (AnswerableMessage a) where
    liftShowsPrec _ _ i (AnswerableMessage a) = ("AnswerableMessage " ++) . showsPrec i a
instance Show (AnswerableMessage a r) where
    showsPrec = liftShowsPrec undefined undefined


answer :: ActorContext p m => r (PRoot p) -> AnswerableMessage r (PRoot p) -> m ()
answer r (AnswerableMessage ref) = ref ! convert r 
