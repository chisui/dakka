{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , MultiParamTypeClasses
           , ExistentialQuantification
           , DeriveDataTypeable
#-}
module Hakka.Actor where

import Data.Proxy ( Proxy(..) )
import Data.Typeable

import Control.Monad.Trans.Writer.Lazy ( WriterT, tell )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )

data ActorRef a = ActorRef String
    deriving (Eq, Show)

data SystemMessage = forall a. Actor a => SystemMessage
    { to  :: ActorRef a
    , msg :: AcceptsMsg a 
    }

type ActorContext = WriterT [SystemMessage] IO

newtype Behavior m = Behavior
    { onMessage :: m -> ActorContext (Behavior m)
    }

class (Eq (AcceptsMsg a), Show (AcceptsMsg a), Typeable a, Typeable (AcceptsMsg a)) => Actor a where
    type AcceptsMsg a
    startBehavior :: Proxy a -> Behavior (AcceptsMsg a)

send :: Actor a => ActorRef a -> AcceptsMsg a -> ActorContext ()
send ref msg = tell [SystemMessage ref msg]


-- Test If Actor can be implemented

data Message = HelloWorld deriving (Eq, Show, Typeable)

data HelloSayer = HelloSayer deriving (Eq, Show, Typeable)
instance Actor HelloSayer where
    type AcceptsMsg HelloSayer = Message
    startBehavior _ = let b = Behavior (\msg -> lift (print msg) >> return b) in b

