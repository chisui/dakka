{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Dakka.AnswerableMessage
    ( AnswerableMessage
    , answerableMessage
    , answer
    ) where

import "base" Data.Typeable ( Typeable )

import "binary" Data.Binary ( Binary(..) )

import Dakka.Actor ( ActorContext(..), ActorMessage, CtxMessage )
import Dakka.Convert ( Convertible(..) )


data AnswerableMessage m c 
  = forall a m. 
    ( Actor a
    , ActorContext a m
    ) => AnswerableMessage (CtxRef m a)

instance Eq (AnswerableMessage m c) where
    _ == _ = False -- LIES
instance Show (AnswerableMessage m c) where
    show _ = "AnswerableMessage"
instance Typeable r => ActorMessage (AnswerableMessage r)
instance Binary (AnswerableMessage m c) where
    put = undefined
    get = undefined

answerableMessage
  :: forall c a m.
    ( ActorContext a m
    ) => CtxRef m a -> AnswerableMessage m c 
answerableMessage = return . AnswerableMessage

answer 
  :: ( Actor a
     , c a
     , ActorContext a m
     , ActorContext b n
     , CtxRef m ~ CtxRef n
     ) => Message a -> AnswerableMessage m c -> n ()
answer m (AnswerableMessage ref) = ref ! m

