{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
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
import "base" GHC.Generics ( Generic )

import "binary" Data.Binary ( Binary )

import Dakka.Actor ( ActorContext(..), ActorMessage )


data AnswerableMessage (r :: (* -> *) -> *) (m :: * -> *)
    = AnswerableMessage
  deriving (Generic)

deriving instance Typeable (AnswerableMessage r m)

instance Eq (AnswerableMessage r m) where
    _ == _ = False -- LIES
instance Show (AnswerableMessage r m) where
    show _ = "AnswerableMessage"
instance Typeable r => ActorMessage (AnswerableMessage r)
instance Binary (AnswerableMessage r m)


answerableMessage :: CtxRef m p -> AnswerableMessage r n 
answerableMessage _ = AnswerableMessage 

answer :: ActorContext m => r m -> AnswerableMessage r m -> m ()
answer = undefined 

