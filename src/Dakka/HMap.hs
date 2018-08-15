{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}
module Dakka.HMap ( HMap, hEmpty, hInsert, hLookup ) where

import "base" Data.Typeable ( Typeable )
import "base" Unsafe.Coerce ( unsafeCoerce )

import "containers" Data.Map ( Map, insert, empty )
import qualified "containers" Data.Map as Map

import Dakka.Constraints ( GEq( geq ), GOrd( gcompare ) ) 


data Key k = forall a. Typeable a => Key (k a)

instance GEq k => Eq (Key k) where
    (Key a) == (Key b) = geq a b

instance GOrd k => Ord (Key k) where
    compare (Key a) (Key b) = gcompare a b

data Elem = forall a. Typeable a => Elem a

newtype HMap k = HMap (Map (Key k) Elem)

hEmpty :: GOrd k => HMap k
hEmpty = HMap empty

hInsert :: (Typeable v, GOrd k) => k v -> v -> HMap k -> HMap k
hInsert k v (HMap m) = HMap $ insert (Key k) (Elem v) m 

hLookup :: (Typeable v, GOrd k) => k v -> HMap k -> Maybe v
hLookup k (HMap m) = (\ (Elem e) -> unsafeCoerce e) <$> Map.lookup (Key k) m


