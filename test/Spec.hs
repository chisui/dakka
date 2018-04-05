{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}


import "base" Data.Proxy ( Proxy(..) )
import "base" Control.Applicative ( Const(..) )

import "dakka" Dakka.MockActorContext ( execMock' )
import "dakka" Dakka.Actor ( RootActor, (</>), ActorRef(..), Actor( onMessage, onSignal ), Signal(..) )

import TestActors ( TestActor, OtherActor, Msg(..) )

main :: IO ()
main = do
  putStrLn "test simple execution"
  print $ execMock' (ARoot @TestActor </> Proxy @OtherActor) (onMessage . Const $ Msg "hello")

  putStrLn "test rootActor"
  print $ execMock' (ARoot @(RootActor '[TestActor, OtherActor])) (onSignal Created)

