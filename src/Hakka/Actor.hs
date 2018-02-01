{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , MultiParamTypeClasses
           , ExistentialQuantification
#-}
module Hakka.Actor where

import Control.Monad.Trans.Writer.Lazy ( WriterT, tell )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )


data ActorRef a

data SystemMessage = forall a. Actor a => SystemMessage
    { to  :: ActorRef a
    , msg :: AcceptsMsg a 
    }

type ActorContext = WriterT [SystemMessage] IO

newtype Behavior m = Behavior
    { onMessage :: m -> ActorContext (Behavior m)
    }

class Actor a where
    type AcceptsMsg a
    startBehavior :: a -> Behavior (AcceptsMsg a)

send :: Actor a => ActorRef a -> AcceptsMsg a -> ActorContext ()
send ref msg = tell [SystemMessage ref msg]


-- Test If Actor can be implemented

data Message = HelloWorld
  deriving 
    ( Eq
    , Show
    )

data HelloSayer = HelloSayer
instance Actor HelloSayer where
    type AcceptsMsg HelloSayer = Message
    startBehavior _ = let b = Behavior (\msg -> lift (print msg) >> return b) in b

