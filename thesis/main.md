---
title: "Dakka: A dependently typed Actor framework in Haskell" 
author:
- Philipp Dargel
date: 2018-??-??
tags:
- Bachelor thesis
- Haskell
- Actor
- Dependent types
geometry: "left=3.5cm, right=2.5cm"
titlepage: yes
toc-own-page: yes
---

# Introduction

The goal of this thesis is to create an Actor framework, simlar to akka for Haskell. It should be possible to reason about a actor system without having to run it to ensure some safty assumtions. To achieve this I will leverage some of haskells dependent typing features. It should also be possible to easily test actor implementations without creating a full actor system. 

# Prior art

## Cloud Haskell

Cloud Haskell is described by its authors as a platform for Erlang-style concurrent and distributed programming in Haskell.

Since Erlang-style concurrency is implemented using the actor model Cloud Haskell already provides a full fledged actor framework for Haskell. In addition there are rich facillites to create distributed Haskell system. It doesn't make creating distributed systems in Haskell easy but is capable of performing the heavy lifting.

Unfortunatly Cloud Haskell has to be somewhat oppinionated since some features it provides wouldn't be possible otherwise. The biggest problem is the fact that Haskell does not provide a way to serialize functions at all. Cloud Haskell solves this through the `distributed-static` package which requires some restrictions in the way functions are defined to work.

# Implementation

In the course of implementation we assume that several language extensions are enabled. When basic extensions like `FlextibleContexts`, `MultiParamTypeClasses` or `PackageImports` or those that only provide syntactic sugar are used it wont be mentioned in the text. If the extension is significant for the code to work it will be mentioned. To take a look at which extensions where used you can run

    grep -Phore "(?<=LANGUAGE )\w+" | sort -u

in the source directory.

## Actor

We need a way to identify specific actors at compile time to be able to reason about them. The best way to do so is by defining types for actors. Since Actors have a state this state type will be the type we will identify the actor with. We could have chosen the message type but the state type seems more descriptive. 

```haskell
data SomeActor = SomeActor
  deriving (Eq, Show, Generic, Binary)
```

Note that we derive `Generic` and `Binary`. This allows the state of an actor to be serialized. 

An actor now has to implement the `Actors` type class. On this typeclass we can ensure that the actor state is serializable and can be printed in human readable form to be included in error messages and log entries.

```haskell
class (Show a, Binary a) => Actor a where
```

The first member of this class will be a typefamily that maps a given actor state type (actor type for short) to a message type this actor can handle. If the message type is not specified it is assumed that the actor only understands `()` as a message.

```haskell
  type Message a
  type Message a = ()
```

To be able to send these messages around in a distributed system we have to ensure that we can send them around. They have to essentially fullfil the same constraints as the actor type itself. For this we create a constraint type alias (possible through the language extension `ConstraintKinds`):

```haskell
type RichData a = (Show a, Binary a)
```

Now the class header can be changed to:

```haskell
class (RichData a, RichData (Message a)) => Actor a where
```

Instead of a constraint tpye alias we could also have used a new class and provided a single `instance (Show a, Binary a) => RichData a`. This would allow `RichData` to be partially applied. There is currently no need to do this though.

Next we have to define a way for actors to handle Messages.

```haskell
  behavior :: Message a -> ActorContext ()
```

`ActorContext` will be a class that provides the actor with a way to perform its actions.

## ActorContext

We need a way for actors to perform their actor operations. To recall actors may

1. send a finite number of messages to other actors.
2. create a finite number of new actors.
3. designate the behavior to be used for the next message it receives. In other words change their internal state.

The most straight forward way to implement these actions would be to use a monad transformer for each action. Creating and sending could be modeled with `WriterT` and chaning the internal state through `StateT`. The innermost monad wont be a tranformer of course.

But here we encounter two issues:

1. to change the sate me must know which actors behavior we are currently describing.
2. to send a message we must ensure that the target actor can handle the message.

The first issue can be solved by adding the actor type to `ActorContext` as a type parameter.

The second is a little trickier.

# Bibliography

