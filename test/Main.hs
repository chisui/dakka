{-# LANGUAGE PackageImports #-}
module Main ( main ) where

import "tasty" Test.Tasty ( defaultMain, testGroup )

import qualified Spec.Dakka.Convert as Convert
import qualified Spec.Dakka.Path as Path 


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Convert.tests
    , Path.tests
    ]
