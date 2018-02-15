{-# LANGUAGE FlexibleContexts
           , ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , StandaloneDeriving
           , DeriveDataTypeable
           , TupleSections
#-}
module Dakka.Actor where

import Type.Reflection ( Typeable, typeOf, splitApps, SomeTypeRep )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Last(..) )
import Data.Bifunctor ( first ) 

import Control.Monad.IO.Class ( MonadIO( liftIO ) )
import Control.Monad.Trans.Writer.Lazy ( WriterT( runWriterT ), tell, execWriterT )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )


-- | Placeholder for real ActorReferences
data ActorRef a = ActorRef String
  deriving
    ( Eq
    , Show
    )

-- | A Message with its associated recipient.
data SystemMessage = forall m. Actor m => SystemMessage
    { to  :: ActorRef m
    , msg :: m 
    }
deriving instance Show SystemMessage

-- | The Environment in which an Actor is executed.
newtype ActorContext m a = ActorContext 
    { ctx :: WriterT (Last (Behavior m), [SystemMessage]) IO a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

-- | Lets an 'Actor' handle the provided message. 
-- This results in a potentially modified ActorInstance with a new behavior and a list of messages the
-- Actor sent during its runtime.
runActor :: forall m. Actor m => ActorInstance m -> m -> IO (ActorInstance m, [SystemMessage])
runActor (ActorInstance b) m = fmap (first applySwitch) $ execWriterT $ ctx $ b m
  where
    applySwitch :: Last (Behavior m) -> ActorInstance m
    applySwitch = ActorInstance . fromMaybe b . getLast

-- | Behavior of an 'Actor'.
type Behavior m = m -> ActorContext m ()

-- | Actors are identified by the type of message they can handle.
class (Show m, Eq m, Typeable m) => Actor m where
    -- | the behavior the 'Actor' starts with.
    startBehavior :: Behavior m

-- | A running Actor with its current behavior.
newtype ActorInstance m = ActorInstance 
    { currentBehavior :: Behavior m 
    }

-- Behavior is not showable, so we show the message type.
instance (Typeable m) => Show (ActorInstance m) where
  show (ActorInstance b) = "ActorInstance(" ++ (show . messageTypeFromBehavior $ b) ++")"

-- | Retrieve the message type from a given Behavior.
messageTypeFromBehavior :: Typeable m => Behavior m -> SomeTypeRep
messageTypeFromBehavior = head . snd . splitApps . typeOf

-- | Creates an instance of given 'Actor' with the startBehavior.
create :: Actor m => ActorInstance m
create = ActorInstance startBehavior

-- | Sends a message to the 'Actor' refered to by given 'ActorRef'.
send :: (Actor s, Actor m) => ActorRef m -> m -> ActorContext s ()
send ref msg = ActorContext $ tell (Last Nothing, [SystemMessage ref msg])

-- | Switch the current behavior with the provided one.
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

