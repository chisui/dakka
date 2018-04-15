{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Dakka.Actor ( tests ) where

import "tasty" Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, (===), Arbitrary(..), oneof ) 

import "dakka" Dakka.Actor


instance Arbitrary ActorId where
    arbitrary = oneof [pure AnyActor, SpecificActor <$> arbitrary] 


tests :: TestTree
tests = testGroup "Dakka.Actor"
    [ testGroup "ActorId"
        [ testGroup "Monoid"
            [ testProperty "0 <> x = x" $
                \ (x :: ActorId) -> mempty <> x === x
            , testProperty "x <> 0 = x" $
                \ (x :: ActorId) -> x <> mempty === x
            , testProperty "x <> (y <> z) = (x <> y) <> z" $
                \ (x :: ActorId) y z -> x <> (y <> z) === (x <> y) <> z
            , testProperty "mconcat = foldr (<>) 0" $
                \ (l :: [ActorId]) -> mconcat l === foldr (<>) mempty l
            ]
        , testGroup "Show"
            [ testCase "show AnyActor = \"any\"" $
                show AnyActor @=? "any"
            , testCase "show (SpecificActor 42) = \"id 42\"" $
                show (SpecificActor 42) @=? "id 42"
            , testCase "show (Just (SpecificActor 42)) = \"Just (id 42)\"" $
                show (Just (SpecificActor 42)) @=? "Just (id 42)"
            ]
        , testGroup "Read"
            [ testCase "read \"any\" = AnyActor" $
                read "any" @=? AnyActor
            , testCase "read \"id 42\" = SpecificActor 42" $
                read "id 42" @=? SpecificActor 42
            , testCase "read \"Just (id 42)\" = Just (SpecificActor 42)" $
                read "Just (id 42)" @=? Just (SpecificActor 42)
            ]
        , testProperty "read . show = id" $
            \ (a :: ActorId) -> (read . show) a === a
        ]
    ]

