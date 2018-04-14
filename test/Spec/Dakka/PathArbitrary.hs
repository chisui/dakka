{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Spec.Dakka.PathArbitrary () where

import "base" GHC.Exts ( IsList(..) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( Arbitrary(..), NonEmptyList(..) )

import "dakka" Dakka.Path


instance Arbitrary a => Arbitrary (Path a) where
    arbitrary = fromList . getNonEmpty <$> arbitrary 

instance Arbitrary a => Arbitrary (HPathT ('Root b) a) where
    arbitrary = IRoot <$> arbitrary 

instance (Arbitrary a, Arbitrary (HPathT bs a)) => Arbitrary (HPathT (bs ':/ b) a) where
    arbitrary = (://) <$> arbitrary <*> arbitrary

