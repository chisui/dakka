{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
#-}
module Dakka.Convert where

class Convertible a b where
    convert :: a -> b

instance Convertible a a where
    convert = id

