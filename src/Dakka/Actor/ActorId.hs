{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
module Dakka.Actor.ActorId where

import "base" GHC.Generics ( Generic )
import "base" Text.ParserCombinators.ReadP ( (+++), readP_to_S, readS_to_P, string )
import "base" Data.Typeable ( Typeable )


data ActorId
    = AnyActor
    | SpecificActor Word
  deriving (Eq, Ord, Typeable, Generic)

instance Semigroup ActorId where
    AnyActor <> b = b
    a <> _ = a

instance Monoid ActorId where
    mempty = AnyActor 

instance Show ActorId where
    showsPrec _ AnyActor = showString "any"
    showsPrec d (SpecificActor i) = showParen (d > 10) $ showString "id " . shows i

instance Read ActorId where
    readsPrec d = readParen (d > 10) $ readP_to_S $ readsAny +++ readsSpecific
      where
        readsAny = do
            _ <- string "any"
            pure AnyActor
        readsSpecific = do
            _ <- string "id" 
            i <- readS_to_P reads
            pure $ SpecificActor i

