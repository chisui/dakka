{-# LANGUAGE PackageImports #-}
import "tasty" Test.Tasty ( defaultMain, testGroup )

import qualified Spec.Dakka.Convert as Convert
import qualified Spec.Dakka.Path as Path 
import qualified Spec.Dakka.Actor as Actor 


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Convert.tests
    , Path.tests
    , Actor.tests
    ]
