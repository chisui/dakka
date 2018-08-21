{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeFamilies          #-}
module Dakka.Convert where

import           "base" Control.Applicative (Const (..))


class Convertible a b where
    convert :: a -> b

instance Convertible a b => Convertible (Const a x) (Const b y) where
    convert = Const . convert . getConst

instance {-# OVERLAPPABLE #-} (a ~ b) => Convertible a b where
    convert = id

