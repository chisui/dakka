{-# LANGUAGE MultiParamTypeClasses #-}
module Dakka.Convert where

class Convertible a b where
    convert :: a -> b

