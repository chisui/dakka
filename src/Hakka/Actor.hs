{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , MultiParamTypeClasses
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
#-}
module Hakka.Actor where

import Data.Monoid ( Last )

import Control.Monad.IO.Class ( MonadIO( liftIO ) )
import Control.Monad.Trans.Writer.Lazy ( WriterT, tell )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )


data ActorRef a

data SystemMessage = forall m. Actor m => SystemMessage
    { to  :: ActorRef m
    , msg :: m 
    }

newtype ActorContext m a = ActorContext (WriterT (Last (Behavior m)) (WriterT [SystemMessage] IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

type Behavior m = m -> ActorContext m ()

class Actor m where
    startBehavior :: Behavior m

send :: (Actor s, Actor m) => ActorRef m -> m -> ActorContext s ()
send ref msg = ActorContext $ lift $ tell [SystemMessage ref msg]

switch :: Actor m => Behavior m -> ActorContext m ()
switch = ActorContext . tell . return


-- Test If Actor can be implemented

newtype Print = Print String
  deriving 
    ( Eq
    , Show
    )

instance Actor Print where
  startBehavior = behavior 0
    where
      behavior i (Print msg) = do
        liftIO $ putStrLn $ show i ++ " " ++ show msg
        switch $ behavior $ i + 1 

newtype Reverse = Reverse String
  deriving 
    ( Eq
    , Show
    )

instance Actor Reverse where
  startBehavior (Reverse msg) = do
    send undefined $ Print $ reverse msg
