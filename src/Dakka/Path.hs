{-# LANGUAGE Safe #-}
-- | Typesafe Paths with type unsafe indices.
module Dakka.Path
    ( Path(..)
    , Tip
    , PRoot
    , IndexedPath(..)
    , root
    , IndexedRef(..)
    , ref
    , AllSegmentsImplement
    , module Exp
    ) where

import Dakka.Path.Base
import Dakka.Path.PathSegment as Exp
import Dakka.Path.IndexedPath

