{-# LANGUAGE TypeApplications
           , PackageImports
           , DataKinds
#-}

import "base" Data.Proxy ( Proxy(..) )
import "base" Control.Applicative ( Const(..) )

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor

import TestActors


main = do
  putStrLn "test simple execution"
  print $ execMock' (ARoot @TestActor </> Proxy @OtherActor) (onMessage . Const $ Msg "hello")

  putStrLn "test rootActor"
  print $ execMock' (ARoot @(RootActor '[TestActor, OtherActor])) (onSignal Created)

