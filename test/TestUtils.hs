{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module TestUtils ( (@~?), testMonoid, testSemigroup ) where

import "base" Data.Typeable ( Typeable, typeRep )
import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( assertEqual, Assertion )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( Arbitrary, testProperty, (===) )

(@~?) :: forall (a :: k0) (b :: k1) proxy0 proxy1. (Typeable a, Typeable b) => proxy0 a -> proxy1 b -> Assertion
a @~? b = assertEqual "" (typeRep a) (typeRep b)
infix 1 @~?

testSemigroupProp :: forall a. (Arbitrary a, Show a, Eq a, Semigroup a) => TestTree
testSemigroupProp = testProperty "x <> (y <> z) = (x <> y) <> z" $
    \ (x :: a) y z -> x <> (y <> z) === (x <> y) <> z

testSemigroup :: forall a. (Arbitrary a, Show a, Eq a, Semigroup a) => TestTree
testSemigroup = testGroup "Semigroup" [testSemigroupProp @a]

testMonoid :: forall a. (Arbitrary a, Show a, Eq a, Monoid a) => TestTree
testMonoid = testGroup "Monoid"
    [ testProperty "0 <> x = x" $
        \ (x :: a) -> mempty <> x === x
    , testProperty "x <> 0 = x" $
        \ (x :: a) -> x <> mempty === x
    , testSemigroupProp @a
    , testProperty "mconcat = foldr (<>) 0" $
        \ (l :: [a]) -> mconcat l === foldr (<>) mempty l
    ]
