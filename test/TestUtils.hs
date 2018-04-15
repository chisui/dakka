{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module TestUtils ( (@~?) ) where

import "base" Data.Typeable ( Typeable, typeRep )
import "tasty-hunit" Test.Tasty.HUnit ( assertEqual, Assertion )

(@~?) :: forall (a :: k0) (b :: k1) proxy0 proxy1. (Typeable a, Typeable b) => proxy0 a -> proxy1 b -> Assertion
a @~? b = assertEqual "" (typeRep a) (typeRep b)
infix 1 @~?

