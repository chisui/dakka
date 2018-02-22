{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , DataKinds
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
import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.Trans.Class ( MonadTrans( lift ) )


data Envelope = Envelope
data NewActor = NewActor

type Behavior s (c :: [*]) m = m -> ActorContext s c ()
newtype ActorContext s (c :: [*]) a = ActorContext (StateT s (WriterT ([Envelope], [NewActor]) IO) a)
  deriving (Functor, Applicative, Monad)

class Actor s m | s -> m where
  type Creates s :: [*]
  type Creates s = '[]
  behavior :: Behavior s (Creates s) m


-- Test --


data TestActor = TestActor

instance Actor TestActor String where
  behavior m = undefined 

