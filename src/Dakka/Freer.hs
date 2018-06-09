{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Dakka.Freer where

import "base" Data.Kind ( Type )
import "base" Control.Monad ( join ) 

import "transformers" Control.Monad.Trans.Except ( ExceptT(..) )
import "transformers" Control.Monad.Trans.Class ( lift )
import qualified "transformers" Control.Monad.Trans.Writer as W

import "freer-simple" Control.Monad.Freer
import "freer-simple" Control.Monad.Freer.Error
import "freer-simple" Control.Monad.Freer.Writer


class Monad m => RunFreer m eff where
  runEff :: Eff (eff ': effs) a -> Eff effs (m a)


instance (Show e, Traversable t, Monoid (t e)) => RunFreer IO (Writer (t e)) where
  runEff = fmap printMsgs . runWriter
    where
      printMsgs (a, msgs) = mapM_ print msgs >> return a

instance Show e => RunFreer IO (Error e) where
  runEff = fmap (either (error . show) pure) . runError



instance {-# overlaps #-} (Monad m, Monoid w) => RunFreer (W.WriterT w m) (Writer w) where
  runEff = fmap (W.WriterT . pure) . runWriter

instance {-# overlappable #-} (RunFreer m eff, Monoid w) => RunFreer (W.WriterT w m) eff where
  runEff = fmap lift . runEff



instance {-# overlaps #-} Monad m => RunFreer (ExceptT e m) (Error e) where
  runEff = fmap (ExceptT . pure) . runError

instance {-# overlappable #-} RunFreer m eff => RunFreer (ExceptT e m) eff where
  runEff = fmap lift . runEff



class Monad m => RunAllFreer m (l :: [Type -> Type]) where
  runAllEff :: Eff l a -> m a


instance Monad m => RunAllFreer m '[] where
  runAllEff = pure . run

instance (RunFreer m eff, RunAllFreer m effs) => RunAllFreer m (eff ': effs) where
  runAllEff = join . runAllEff . runEff


action :: Eff '[Error String, Writer [String]] Int
action = do
    tell ["hello", "world"]
    _ <- throwError "noooo"
    tell ["goodby"]
    return 0
