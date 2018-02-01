{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, GADTs #-}
module Hakka.Actor where

import Data.Proxy


newtype Behavior m = Behavior
    { handle :: m -> IO (Behavior m)
    }

class Actor a where
    type AcceptsMsg a
    startBehavior :: Proxy a -> Behavior (AcceptsMsg a)

data ActorRef a
send :: Actor a => ActorRef a -> (AcceptsMsg a) 
send = undefined
