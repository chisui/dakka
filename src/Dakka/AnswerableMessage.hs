{-# LANGUAGE ExistentialQuantification
           , KindSignatures
           , PackageImports
           , StandaloneDeriving
           , FlexibleContexts
           , TypeOperators
           , DataKinds
#-}
module Dakka.AnswerableMessage where

import "base" Data.Typeable ( Typeable, cast, TypeRep )

import Dakka.Actor
import Dakka.Type.Path
import Dakka.Type.Tree
import Dakka.Convert


data AnswerableMessage r = forall (p :: Path *). (ConsistentActorPath p, Actor (Tip p), Convertible r (Message (Tip p))) => AnswerableMessage
    { askerRef :: ActorRef p
    }

deriving instance Typeable r => Typeable (AnswerableMessage r)

instance Eq (AnswerableMessage r) where
    (AnswerableMessage a) == (AnswerableMessage b) = demotePath a == demotePath b
      where
        demotePath :: ActorRef p -> Path (Word, TypeRep)
        demotePath = convert

instance Show (AnswerableMessage r) where
    show (AnswerableMessage a) = "AnswerableMessage " ++ show a

answer :: ActorContext p m => r -> AnswerableMessage r -> m ()
answer r (AnswerableMessage ref) = ref ! convert r 

