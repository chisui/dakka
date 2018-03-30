{-# LANGUAGE TypeApplications
           , PackageImports
           , DataKinds
#-}

import "base" Data.Proxy ( Proxy(..) )

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor
import "dakka" Dakka.Type.Path

import TestActors


main = do
  putStrLn "test simple execution"
  print $ execMock' (ARoot @TestActor </> Proxy @OtherActor) (onMessage $ Msg "hello")

  putStrLn "test rootActor"
  print $ execMock' (ARoot @(RootActor '[TestActor, OtherActor])) (onSignal Created)

