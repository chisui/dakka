{-# LANGUAGE TypeApplications
           , PackageImports
           , DataKinds
#-}

import "base" Data.Proxy ( Proxy(..) )

import "dakka" Dakka.MockActorContext
import "dakka" Dakka.Actor
import "dakka" Dakka.Type.Path

import TestActors


main = print $ execMock' @(ActorSystemTree '[TestActor]) (Proxy @TestActor </> Proxy @OtherActor) (behavior $ Msg "hello")

