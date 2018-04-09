{-# LANGUAGE Safe #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Dakka.Path.PathSegment where

import "base" Control.Applicative ( Const(..) )
import "base" Data.Proxy ( Proxy )

import Dakka.Path.Base
import Dakka.Path.IndexedPath

class PathSegment a b c | a b -> c where
    (</>) :: a -> b -> c

instance PathSegment (Path a) a (Path a) where
    (</>) = (:/)

instance (PathSegment a b c, Functor f) => PathSegment (f a) b (f c) where
    f </> a = (</> a) <$> f

instance PathSegment (IndexedPath as) (Proxy a) (IndexedPath (as ':/ a)) where
    p </> _ = p :// 0
instance PathSegment (IndexedPath as) (IndexedRef a) (IndexedPath (as ':/ a)) where
    p </> (IR i) = p :// i
instance PathSegment (IndexedPath as) (Const Word a) (IndexedPath (as ':/ a)) where
    p </> (Const i) = p :// i

