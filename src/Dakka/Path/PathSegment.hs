{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Dakka.Path.PathSegment where

import "base" Control.Applicative ( Const(..) )
import "base" Data.Proxy ( Proxy )

import Dakka.Path.Base
import Dakka.Path.HPathT

class PathSegment a b where
    type ResultPath a b
    type ResultPath a b = a
    (</>) :: a -> b -> ResultPath a b

instance PathSegment (Path a) a where
    (</>) = (:/)

(</$>) :: (PathSegment a b, Functor f) => f a -> b -> f (ResultPath a b)
f </$> a = (</> a) <$> f

instance Monoid b
         => PathSegment (HPathT as b) (Proxy a) where
    type ResultPath (HPathT as b) (Proxy a) = (HPathT (as ':/ a) b)
    p </> _ = p :// mempty

instance {-# OVERLAPS #-}
         PathSegment (HPathT as b) (Const b a) where
    type ResultPath (HPathT as b) (Const b a) = (HPathT (as ':/ a) b)
    p </> (Const i) = p :// i

