{-# LANGUAGE TypeOperators
           , DeriveDataTypeable
           , TypeFamilies
           , UndecidableInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , FlexibleInstances
           , TypeSynonymInstances
           , GADTs
           , FlexibleContexts
           , StandaloneDeriving
#-}
module Hakka.Tree where

import Data.Proxy ( Proxy(..) )
import Data.Typeable ( Typeable(..), cast )
import Data.Maybe ( fromJust )
import Data.Foldable ( asum )
import Control.Applicative ( Alternative(..), liftA2 )


data Nothing deriving Typeable

-- heterogenous list
data a :++: b = a :++: b deriving (Eq, Show, Typeable)
infixr 5 :++:
data Nil = Nil deriving (Eq, Show, Typeable)

-- Find by type in List
type family FindA a b where
  FindA a (a :++: b) = a
  FindA a (b :++: c) = FindA a c
  FindA a b = Nothing

class Findable a b where
  findA :: a -> b -> FindA a b

instance {-# OVERLAPPABLE #-} (a ~ FindA a (a :++: b)) => Findable a (a :++: b) where
  findA _ (a :++: _) = a
instance {-# OVERLAPS #-} (a ~ FindA a t, a ~ FindA a (x :++: t), Findable a t) => Findable a (x :++: t) where
  findA a (_ :++: b) = findA a b

-- Last
type family Last b where
  Last (a :++: Nil) = a
  Last (a :++: b)   = Last b
  Last a = Nothing

class HasLast a where
  hlast :: a -> Last a

instance {-# OVERLAPPABLE #-} HasLast (a :++: Nil) where
  hlast (a :++: _) = a
instance {-# OVERLAPS #-} (HasLast b, Last b ~ Last (a :++: b)) => HasLast (a :++: b) where
  hlast (_ :++: b) = hlast b


-- Test --

data A = A deriving (Eq, Show, Typeable)
_A = Proxy :: Proxy A
data B = B deriving (Eq, Show, Typeable)
_B = Proxy :: Proxy B
data C = C deriving (Eq, Show, Typeable)
_C = Proxy :: Proxy C
data D = D deriving (Eq, Show, Typeable)
_D = Proxy :: Proxy D
data E = E deriving (Eq, Show, Typeable)
_E = Proxy :: Proxy E
data F = F deriving (Eq, Show, Typeable)
_F = Proxy :: Proxy F

l = A :++: B :++: C :++: Nil

