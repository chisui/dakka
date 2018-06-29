{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Dakka.AnswerableMessage
    ( AnswerableMessage
    , ConstraintAnswerableMessage
    , answerableMessage
    , answer
    ) where

import "base" Data.Kind ( Constraint )

import "binary" Data.Binary ( Binary(..) )

import Dakka.Actor ( ActorContext(..), Actor( Message ), ActorRef )
import Dakka.Constraints ( (=~=) )
import Dakka.Convert ( Convertible(..) ) 

data ConstraintAnswerableMessage (m :: *) (c :: * -> Constraint)
  = forall a. 
    ( Actor a
    , c a
    , Convertible m (Message a)
    ) => AnswerableMessage (ActorRef a)

instance Eq (ConstraintAnswerableMessage m c) where
    (AnswerableMessage refA) == (AnswerableMessage refB) = refA =~= refB
instance Show (ConstraintAnswerableMessage m c) where
    showsPrec d (AnswerableMessage ref) = showParen (d > 10) 
                                        $ showString "AnswerableMessage"
                                        . shows ref
instance Binary (ConstraintAnswerableMessage m c) where
    put = undefined
    get = undefined

type AnswerableMessage m = ConstraintAnswerableMessage m Actor 

answerableMessage
  :: forall (a :: *) (m :: *) (c :: * -> Constraint). 
    ( Actor a
    , c a
    , Convertible m (Message a)
    ) => ActorRef a -> ConstraintAnswerableMessage m c
answerableMessage = AnswerableMessage

answer :: ActorContext b ctx => m -> ConstraintAnswerableMessage m c -> ctx () 
answer m (AnswerableMessage ref) = ref ! convert m

