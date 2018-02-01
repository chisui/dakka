module Hakka.Example where

import Hakka.Actor

helloWorldActor :: String -> ActorContext String -> IO (ActorRef String)
helloWorldActor name context = newActor context name behavior where
  behavior = Behavior receive
  receive context (Signal (ChildFailed p e)) = do
    logMessage context Error $ "ChildFailed failed: " ++ (show p) ++ ": " ++ e
    return behavior
  receive context (Signal Stop) = return Stopped
  receive context (Message text) = do
    let (!) = sendMessage context
    logMessage context Info $ "Hello " ++ text ++ " from " ++ name
    case name of
      "hello1" -> return ()
      "hello2" -> do
        actorA2 <- helloWorldActor "hello3" context
        actorA2 ! "blub"
      "hello3" -> do
        logMessage context Warn $ "I am about to fail"
        error "oh no!"
    return behavior

example = do
  system <- actorSystem "example"
  let context = root system
  let (!) = sendMessage context
  actorA <- helloWorldActor "hello1" context
  actorB <- helloWorldActor "hello2" context
  actorA ! "world 1"
  actorB ! "world 2"
  actorA ! "Hello?"