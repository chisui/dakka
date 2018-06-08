{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
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


data AnswerableMessage (r :: (* -> *) -> *) (m :: * -> *)
  = forall n. 
    ( ActorContext n
    , CtxRef m ~ CtxRef n
    , Binary (CtxMessage n)
    , r n `Convertible` CtxMessage n
    ) => AnswerableMessage (CtxRef n (CtxPath n))

deriving instance Typeable (AnswerableMessage r m)

instance Eq (AnswerableMessage r m) where
    _ == _ = False -- LIES
instance Show (AnswerableMessage r m) where
    show _ = "AnswerableMessage"
instance Typeable r => ActorMessage (AnswerableMessage r)
instance Binary (AnswerableMessage r m) where
    put = undefined
    get = undefined

answerableMessage
  :: ( ActorContext m
     , ActorContext n
     , Binary (CtxMessage n)
     , CtxRef m ~ CtxRef n
     , r n `Convertible` CtxMessage n
     ) => CtxRef m (CtxPath n) -> m (AnswerableMessage r n)
answerableMessage = return . AnswerableMessage

answer :: ActorContext m => r m -> AnswerableMessage r m -> m ()
answer r (AnswerableMessage ref) = ref ! convert r 

