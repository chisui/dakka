{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
module Dakka.Types
    ( showsType
    , (=~=)
    , GEq(..)
    , GOrd(..)
    ) where


import           "base" Data.Kind     (type (*))
import           "base" Data.Proxy    (Proxy (..))
import           "base" Data.Typeable (Typeable, cast, typeRep)


showsType :: forall a. Typeable a => ShowS
showsType = showString "<<"
          . shows (typeRep (Proxy @a))
         . showString ">>"


(=~=) :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
a =~= b = Just a == cast b

class GEq (f :: k -> *) where
    geq :: (Typeable a, Typeable b) => f a -> f b -> Bool
    default geq :: (GOrd f, Typeable a, Typeable b) => f a -> f b -> Bool
    geq a b = gcompare a b == EQ

instance GEq Proxy where

class GEq f => GOrd (f :: k -> *) where
    gcompare :: (Typeable a, Typeable b) => f a -> f b -> Ordering

instance GOrd Proxy where
    gcompare a b = typeRep a `compare` typeRep b
