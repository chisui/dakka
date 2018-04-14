{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Spec.Dakka.Path ( tests ) where

import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Typeable ( typeOf ) 
import "base" GHC.Exts ( IsList(..))
import "base" Control.Monad ( (>=>) ) 
import "base" Control.Applicative ( Const(..) ) 

import "tasty"            Test.Tasty ( testGroup, TestTree )
import "tasty-hunit"      Test.Tasty.HUnit ( testCase, (@=?) )
import "tasty-quickcheck" Test.Tasty.QuickCheck ( testProperty, NonEmptyList(..), (===), Fun, applyFun )

import "dakka" Dakka.Path
import "dakka" Dakka.Convert

import Spec.Dakka.PathArbitrary ()

testPRoot :: ( (a ~ PRoot ('Root a))  
             , (a ~ PRoot ('Root a ':/ Int ':/ String)) 
             ) => a
testPRoot = undefined


testTip :: ( (a ~ Tip ('Root a))
           , (a ~ Tip ('Root String ':/ Int ':/ a))
           ) => a
testTip = undefined


testPop :: ( ('Root Int ':/ String) ~ Pop ('Root Int ':/ String ':/ Word)
           ) => ()
testPop = ()


tests :: TestTree
tests = testGroup "Dakka.Path"
    [ testGroup "Path"
        [ testGroup "IsList"
            [ testProperty "toList . fromList = id" $
                \ (NonEmpty l) -> toList @(Path Int) (fromList l) == l
            , testProperty "fromList . toList = id" $
                \ p -> fromList @(Path Int) (toList p) == p
            ]
        , testGroup "Semigroup"
            [ testProperty "x <> (y <> z) = (x <> y) <> z" $
                \ (x :: Path Int) y z -> x <> (y <> z) === (x <> y) <> z
            , testProperty "x <> y = fromList (toList x <> toList y)" $
                \ (x :: Path Int) y -> x <> y === fromList (toList x <> toList y)
            ]
        , testGroup "Monad"
            [ testProperty "pure a >>= k = k a" $
                \ a (k' :: Int `Fun` Path Char) -> let k = applyFun k' in
                                                       (pure a >>= k) === k a
            , testProperty "m >>= pure = m" $
                \ (m :: Path Int) -> (m >>= pure) === m
            , testProperty "m >>= (k >=> h) = (m >>= k) >>= h" $
                \ (m :: Path Int) k' h' ->
                    let k = (applyFun k' :: Int -> Path Word)
                        h = (applyFun h' :: Word -> Path Word)
                    in (m >>= (k >=> h)) === ((m >>= k) >>= h)
            ]
        , testGroup "Show"
            [ testCase "show (Root 'a') = \"/'a'/\"" $
                show (Root 'a') @=? "/'a'/"
            , testCase "show (Root 'a' :/ 'b' :/ 'c') = \"/'a'/'b'/'c'/\"" $
                show (Root 'a' :/ 'b' :/ 'c') @=? "/'a'/'b'/'c'/"
            , testCase "show (Root (Just 'a')) = \"/(Just 'a')/\"" $
                show (Root (Just 'a')) @=? "/(Just 'a')/"
            , testCase "show (Root (Root 'a') :/ (Root 'b' :/ 'c')) = \"/(/'a'/)/(/'b'/'c'/)/\"" $
                show (Root (Root 'a') :/ (Root 'b' :/ 'c')) @=? "/(/'a'/)/(/'b'/'c'/)/"
            ]
        , testGroup "PathSegment"
            [ testGroup "PathSegment (Path a) a"
                [ testProperty "as </> a = as :/ a" $
                    \ (as :: Path Int) a -> as </> a === as :/ a
                ]
            ]
        ]
    , testGroup "HPathT"
        [ testGroup "Convertible (HPathT p a) (Path (TypeRep, a))"
            [ testCase "convert (root' @Int 'a') = /(Int, 'a')/" $
                convert (root' @Int 'a') @=? Root (typeOf @Int undefined, 'a')
            ]
        , testGroup "Eq"
            [ testProperty "root' @() a </> ref @() b = root' @() a </> ref @() b" $
                \ (a :: Int) (b :: Int) -> root' @() a </> ref @() b === root' @() a </> ref @() b
            , testProperty "(root' @() a = root' @() b) = (a = b)" $
                \ (a :: Int) (b :: Int) -> (root' @() a == root' @() b) === (a == b)
            ]
        , testGroup "Show"
            [ testCase "show (root' @Int 'a') = \"/Int:'a'/\"" $
                show (root' @Int 'a') @=? "/Int:'a'/"
            , testCase "show (root' @(Maybe Int) 'a') = \"/(Maybe Int):'a'/\"" $
                show (root' @(Maybe Int) 'a') @=? "/(Maybe Int):'a'/"
            , testCase "show (root' @(Maybe Int) 'a' </> ref @Word 'b') = \"/(Maybe Int):'a'/Word:'b'/\"" $
                show (root' @(Maybe Int) 'a' </> ref @Word 'b') @=? "/(Maybe Int):'a'/Word:'b'/"
            , testCase "show (root' @Int (Just 'a')) = \"/Int:(Just 'a')/\"" $
                show (root' @Int (Just 'a')) @=? "/Int:(Just 'a')/"
            ]
        , testGroup "PathSegment"
            [ testGroup "PathSegment (HPathT as b) (Proxy a)"
                [ testProperty "as </> a = as :// mempty" $
                    \ (as :: HPathT ('Root Int) ()) -> as </> Proxy @() === as :// mempty
                ]
            , testGroup "PathSegment (HPathT as b) (Const b a)"
                [ testProperty "as </> a = as :// mempty" $
                    \ (as :: HPathT ('Root Int) Char) (a :: Const Char Word) -> as </> a === as :// getConst a 
                ]
            ]
        , testGroup "root"
            [ testCase "demotePath (root' @Int 'a') = Root (Int, 'a')" $
                demotePath (root' @Int 'a') @=? Root (typeOf @Int undefined, 'a')
            , testCase "demotePath (root @Int @()) = Root (Int, ())" $
                demotePath (root @Int @()) @=? Root (typeOf @Int undefined, ())
            ]
        , testGroup "demote"
            [ testCase "typeOfTip (root @Int @()) = Int" $
                typeOfTip (root @Int @()) @=? typeOf @Int undefined 
            , testCase "typeOfTip (root @() @() </> ref @Int ()) = Int" $
                typeOfTip (root @() @() </> ref @Int ()) @=? typeOf @Int undefined 
            , testCase "demoteTip (root @Int @()) = (Int, ())" $
                demoteTip (root @Int @()) @=? (typeOf @Int undefined, ())
            , testCase "demoteTip (root @() @() </> ref @Int ()) = (Int, ())" $
                demoteTip (root @() @() </> ref @Int ()) @=? (typeOf @Int undefined, ())
            , testCase "demotePath (root @Int @()) = Root (Int, ())" $
                demotePath (root @Int @()) @=? Root (typeOf @Int undefined, ())
            , testCase "demotePath (root @Word @() </> ref @Int ()) = Root (Word, ()) :/ (Int, ())" $
                demotePath (root @Word @() </> ref @Int ()) @=? Root (typeOf @Word undefined, ()) :/ (typeOf @Int undefined, ())
            ]
        ]
    , testGroup "PathSegment"
        [ testGroup "</$>"
            [ testProperty "Path" $
                \ (as :: Path Int) (a :: Int) -> Just as </$> a === Just (as </> a)
            , testProperty "HPathT Proxy" $
                \ (as :: HPathT ('Root Int) ()) -> Just as </$> Proxy @Word === Just (as </> Proxy @Word)
            , testProperty "HPathT Const" $
                \ (as :: HPathT ('Root Int) Char) (a :: Char) -> Just as </$> ref @Word a === Just (as </> ref @Word a)
            ]
        ]
    ]

