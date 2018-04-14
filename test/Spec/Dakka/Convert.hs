{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.Dakka.Convert ( tests ) where

import "base" Control.Applicative ( Const(..) )

import "tasty"       Test.Tasty ( testGroup, TestTree )
import "tasty-hunit" Test.Tasty.HUnit ( testCase, (@=?) )

import "dakka" Dakka.Convert

newtype A a = A a deriving (Show, Eq)
newtype B a = B a deriving (Show, Eq)

instance Convertible a b => Convertible (A a) (B b) where
    convert (A a) = B $ convert a


tests :: TestTree
tests = testGroup "Dakka.Convert"
    [ testCase "fallback identity instance" ("hello" @=? convert "hello")
    , testCase "Custom types" (B "hello" @=? convert (A "hello"))
    , testGroup "Const" 
        [ testCase "convert with identit" (Const @Int @String 1 @=? convert (Const @Int @Int 1))
        , testCase "convert with Convertible" (Const @_ @String (B "hello") @=? convert (Const @_ @Int (A "hello")))
        ]
    ]

