{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Dakka.AnswerableMessage where

import "base" Data.Typeable ( Typeable )

import Dakka.Actor ( ActorRefConstraints, Message, ActorContext( (!), Ref ), ActorRef(..), ActorMessage(..) )
import Dakka.Path ( Path, PRoot, Tip )
import Dakka.Convert ( Convertible( convert ) )


data AnswerableMessage (r :: (Path * -> *) -> Path * -> *) (ref :: Path * -> *) (p :: Path *)
  = forall (q :: Path *).
    ( ActorRefConstraints q
    , PRoot p ~ PRoot q
    , ActorRef ref
    , Convertible (r ref p) (Message (Tip q) ref q)
    ) => AnswerableMessage
        { askerRef :: ref q
        }

deriving instance (Typeable r, Typeable ref, Typeable p) => Typeable (AnswerableMessage r ref p)

instance Typeable r => ActorMessage (AnswerableMessage r)

instance Eq (AnswerableMessage r ref p) where
    AnswerableMessage a == AnswerableMessage b = a `eqRef` b

instance Show (AnswerableMessage r ref p) where
    showsPrec d (AnswerableMessage a) = showParen (d > 10) $ ("AnswerableMessage " ++) . showsRef 11 a


answer :: ActorContext p m => r (Ref m) p -> AnswerableMessage r (Ref m) p -> m ()
answer r (AnswerableMessage ref) = ref ! convert r 

