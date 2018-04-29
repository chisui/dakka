{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

import Dakka.Actor ( ActorContext(..), ActorMessage )


data AnswerableMessage (r :: (* -> *) -> *) (m :: * -> *)
    = AnswerableMessage
  deriving (Eq, Show, Typeable)

deriving instance Typeable (AnswerableMessage r m)

instance Typeable r => ActorMessage (AnswerableMessage r)


answerableMessage :: ref (CtxPath m) -> AnswerableMessage r m 
answerableMessage _ = AnswerableMessage

answer :: ActorContext p m => r m -> AnswerableMessage r m -> m ()
answer = undefined 

