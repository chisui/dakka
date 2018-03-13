{-# LANGUAGE ExistentialQuantification
           , PackageImports
           , StandaloneDeriving
           , FlexibleContexts
#-}
module Dakka.AnswerableMessage where

import "base" Data.Typeable ( Typeable )

import Dakka.Actor
import Dakka.Type.Path
import Dakka.Convert


data AnswerableMessage r = forall p. (Actor (Tip p), Convertible r (Message (Tip p))) => AnswerableMessage
    { askerRef :: ActorRef p
    }

deriving instance Typeable r => Typeable (AnswerableMessage r)

instance Eq (AnswerableMessage r) where
    (AnswerableMessage a) == (AnswerableMessage b) = demotePath a == demotePath b

instance Show (AnswerableMessage r) where
    show (AnswerableMessage a) = "AnswerableMessage " ++ show a

answer :: (Actor a, ActorContext t p a m) => r -> AnswerableMessage r -> m ()
answer r (AnswerableMessage ref) = ref ! convert r

