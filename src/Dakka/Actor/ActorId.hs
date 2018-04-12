{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Dakka.Actor.ActorId where

import Data.Typeable ( Typeable )


data ActorId
    = AnyActor
    | SpecificActor Word
  deriving (Eq, Typeable)
instance Semigroup ActorId where
    AnyActor <> b = b
    a <> _ = a
instance Monoid ActorId where
    mempty = AnyActor 
instance Show ActorId where
    showsPrec _ AnyActor = showString "any"
    showsPrec d (SpecificActor i) = showParen (d > 10) $ showString "id " . shows i

