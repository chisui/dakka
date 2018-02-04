{-# LANGUAGE FlexibleContexts
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , StandaloneDeriving
           , DeriveDataTypeable
           , TupleSections
#-}
module Hakka.Actor where

import Type.Reflection ( Typeable, typeOf, splitApps, SomeTypeRep )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Last(..) )
import Data.Bifunctor ( first ) 

import Control.Monad.IO.Class ( MonadIO( liftIO ) )
import Control.Monad.Trans.Writer.Lazy ( WriterT( runWriterT ), tell, execWriterT )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )


data ActorRef a = ActorRef String
  deriving
    ( Eq
    , Show
    )

data SystemMessage = forall m. Actor m => SystemMessage
    { to  :: ActorRef m
    , msg :: m 
    }
deriving instance Show SystemMessage

newtype ActorContext m a = ActorContext 
    { ctx :: WriterT (Last (Behavior m), [SystemMessage]) IO a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

runActor :: forall m. Actor m => ActorInstance m -> m -> IO (ActorInstance m, [SystemMessage])
runActor (ActorInstance b) m = fmap (first applySwitch) $ execWriterT $ ctx $ b m
  where
    applySwitch :: Last (Behavior m) -> ActorInstance m
    applySwitch = ActorInstance . fromMaybe b . getLast

type Behavior m = m -> ActorContext m ()

class (Show m, Eq m, Typeable m) => Actor m where
    startBehavior :: Behavior m

newtype ActorInstance m = ActorInstance 
    { currentBehavior :: Behavior m
    }
instance (Typeable m) => Show (ActorInstance m) where
  show (ActorInstance b) = "ActorInstance(" ++ (show . messageTypeFromBehavior $ b) ++")"

messageTypeFromBehavior :: Typeable m => Behavior m -> SomeTypeRep
messageTypeFromBehavior = head . snd . splitApps . typeOf

create :: Actor m => ActorInstance m
create = ActorInstance startBehavior

send :: (Actor s, Actor m) => ActorRef m -> m -> ActorContext s ()
send ref msg = ActorContext $ tell (Last Nothing, [SystemMessage ref msg])

switch :: Actor m => Behavior m -> ActorContext m ()
switch = ActorContext . tell . (,[]) . return


-- Test If Actor can be implemented

newtype Print = Print String
  deriving 
    ( Eq
    , Show
    , Typeable
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
    , Typeable
    )

instance Actor Reverse where
  startBehavior (Reverse msg) = do
    send (ActorRef "someActor") $ Print $ reverse msg

