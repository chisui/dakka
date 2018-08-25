{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE Trustworthy               #-}
module Dakka.HMap
    ( HMap
    , hEmpty
    , hInsert
    , hLookup
    ) where

import           "base" Data.Typeable  (Typeable)
import           "base" Unsafe.Coerce  (unsafeCoerce)

import           "containers" Data.Map (Map, empty, insert)
import qualified "containers" Data.Map as Map

import           Dakka.Types           (GEq (geq), GOrd (gcompare), (=~=))


data Key k where
    Key :: (Typeable a, Eq (k a), Show (k a)) => k a -> Key k

instance Show (Key k) where
    showsPrec d (Key k) = showsPrec d k

instance GEq k => Eq (Key k) where
    (Key a) == (Key b) = geq a b

instance GOrd k => Ord (Key k) where
    compare (Key a) (Key b) = gcompare a b

data Elem where
    Elem :: (Typeable a, Eq a, Show a) => a -> Elem

instance Show Elem where
    showsPrec d (Elem a) = showsPrec d a

instance Eq Elem where
    Elem a == Elem b = a =~= b

type HMapConstrains k v = (GOrd k, Typeable v, Eq v, Eq (k v), Show v, Show (k v))
newtype HMap k = HMap (Map (Key k) Elem)
    deriving Eq

instance Show (HMap k) where
    showsPrec d (HMap m) = showsPrec d m

hEmpty :: GOrd k => HMap k
hEmpty = HMap empty

hInsert :: HMapConstrains k v => k v -> v -> HMap k -> HMap k
hInsert k v (HMap m) = HMap $ insert (Key k) (Elem v) m

hLookup :: HMapConstrains k v => k v -> HMap k -> Maybe v
hLookup k (HMap m) = (\ (Elem e) -> unsafeCoerce e) <$> Map.lookup (Key k) m


