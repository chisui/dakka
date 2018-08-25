{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE PackageImports #-}
module Dakka.Actor.ActorRef where

import           "base" Data.Typeable              (Typeable, typeRep)
import           "base" GHC.Generics               (Generic)

import           "binary" Data.Binary              (Binary)

import           "bytestring" Data.ByteString.Lazy (ByteString)

import           Dakka.Types                       (GEq (..), GOrd (..))


newtype ActorRef a = ActorRef { actorId :: ByteString }
  deriving (Eq, Ord, Generic, Binary)

instance Typeable a => Show (ActorRef a) where
    showsPrec d ref@(ActorRef aId) = showParen (d > 10)
                                   $ showString "ActorRef <<"
                                   . shows (typeRep ref)
                                   . showString ">>@"
                                   . shows aId

instance GEq ActorRef where
instance GOrd ActorRef where
    gcompare refA@(ActorRef idA) refB@(ActorRef idB) = typeRep refA `compare` typeRep refB <> idA `compare` idB

