{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , GADTs
           , RankNTypes
           , MultiParamTypeClasses
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
#-}
module Hakka.Actor where

import Data.Proxy ( Proxy(..) )

import Control.Monad.Trans.Writer.Lazy ( WriterT )

data ActorPath a
instance ActorRef ActorPath where 
data SystemMessage = forall a. (Actor a) => SystemMessage
    { to  :: ActorPath a
    , msg :: AcceptsMsg a 
    }
type ActorContext = WriterT [SystemMessage] IO

newtype Behavior m = Behavior
    { onMessage :: m -> ActorContext (Behavior m)
    }

class Accepts a m where
class (Eq (AcceptsMsg a), Accepts a (AcceptsMsg a)) => Actor a where
    type AcceptsMsg a
    startBehavior :: Proxy a -> Behavior (AcceptsMsg a)

class ActorRef (r :: * -> *) where
send :: (ActorRef r, Actor a) => r a -> (AcceptsMsg a) -> ActorContext ()
send = undefined
