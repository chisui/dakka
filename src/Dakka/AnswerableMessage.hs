{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Dakka.AnswerableMessage where

import "base" Data.Typeable ( Typeable, TypeRep )
import "base" Data.Functor.Classes ( Eq1(..), Show1(..) )

import Dakka.Actor ( ActorRefConstraints, Actor, Message, ActorRef, ActorContext( (!) ) )
import Dakka.Path ( Path, PRoot, Tip )
import Dakka.Convert ( Convertible( convert ) )


data AnswerableMessage r a = forall (p :: Path *).
    ( ActorRefConstraints p
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
        demotePath :: ActorRefConstraints p => ActorRef p -> Path (TypeRep, Word)
        demotePath = convert

instance Eq (AnswerableMessage a r) where
    (==) = liftEq undefined

instance Show1 (AnswerableMessage a) where
    liftShowsPrec _ _ d (AnswerableMessage a) = showParen (d > 10) $ ("AnswerableMessage " ++) . showsPrec 0 a
instance Show (AnswerableMessage a r) where
    showsPrec = liftShowsPrec undefined undefined


answer :: ActorContext p m => r (PRoot p) -> AnswerableMessage r (PRoot p) -> m ()
answer r (AnswerableMessage ref) = ref ! convert r 
