---
title: "Dakka: A dependently typed Actor framework for Haskell" 
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

But here we encounter several issues:

1. to change the sate me must know which actors behavior we are currently describing.
2. to send a message we must ensure that the target actor can handle the message.
3. to create an actor we have to pass around some reference to the actor type of the actor to create. 

The first issue can be solved by adding the actor type to `ActorContext` as a type parameter.

The second and third are a little trickier. To be able to send a message in a typesafe way we need to retain the actor type. But if we would make the actor type explicit in the `WriterT` type we would only be able to send messages to actors of that exact type. Luckily there is a way to get both. Using the language extension `ExistentialQuantification` we can capture the actor type with a constructor without exposing it. To retrieve the captured type you just have to pattern match on the constructor. We can also use this to close over the the actor type in the create case. With this we can create a wrapper for a send and create actions:

```haskell
data SystemMessage
    = forall a. Actor a => Send (ActorRef a) (Message a)
    | forall a. Actor a => Create (Proxy a)
  deriving (Eq, Show)
```

`ActorRef` is some way to identify an actor inside a actor system. We will define it later

Unfortunatly we can't derive `Generic` for data types that use existential quantification and thus can't get a `Binary` instance for free. But as we will later discover we do not need to serialize values of `SystemMessage` so this is fine for now.

With all this we can define `ActorContext` as follows:

```haskell
newtype ActorContext a v 
    = ActorContext (StateT a (Writer [SystemMessage]) v)
  deriving (Functor, Applicative, Monad, MonadWriter [SystemMessage], MonadState a)
```

Notice that we only need one `Writer` since we combined create and send actions into a single type. Since `ActorContext` is nothing more than the composition of several Monad transformers it is itself a monad. Using `GeneralizedNewtypeDeriving` we can derive several useful monad instances. the classes `MonadWriter` and `MonadState` are provided by the `mtl` package.

Since we added the actor type to the signature of `ActorContext` we need to change definition of `behavior` to reflect this:

```haskell
  behavior :: Message a -> ActorContext a () 
```

By derving `MonadState` we get a variety of functions to change the actors state. The other actor actions can now be defined as functions:

### send

```haskell
send :: Actor a => ActorRef a -> Message a -> ActorContext b () 
send ref msg = tell [Send ref msg]
```

Notice that the resulting `ActorContext` doesn't have `a` as its actor type but rather some other type `b`. This is because these two types don't have to be the same one. `a` is the type of actor the message is sent to and `b` is the type of actor whos behavior we are currently describing. The `send` function does not have a `Actor b` constraint since this would needlessly restrict the use of the function itself. When defining an actor it is already ensured that whatever `b` is it will be an `Actor`.

We can also provide an akka-style send operator as a convenient alias for `send`:

```haskell
(!) = send
```

### create

```haskell
create' :: Actor a => Proxy a -> ActorContext b ()
create' a = tell [Create a]
```

As idicated by the `'` this version of create is not intendet to be the main one. For that we define:

```haskell
create :: forall a. Actor a => ActorContext b ()
create = create' (Proxy @a)
```

In combination with `TypeApplications` this enables us to create actors by just writing `create @TheActor` instead of the cumbersome `create' (Proxy :: Proxy TheActor)`.

### Flexibillity and Effects

By defining `ActorContext` as a datatype we force any environment to use exactly this datatype. This is problematic since actors now can only perform their three actor actions. `ActorContext` isn't flexible enough to express anything else. We could change the definition of `ActorContext` to be a monad transformer over `IO` and provide a `MonadIO` instance. This would defeat our goal to be able to reason about actors though since we could now perform any `IO` we wanted.

Luckily Haskells typesystem is expressive enough to solve this problem. Due to this expressiveness there is a myriad of different solutions for this problem though. Not all of them are viable of course. We will take a look at two approaches that integrate well into existing programming paradigms used in haskell and other functional languages.

#### mtl style monad classes

In this approach we 

#### the Eff monad

# Bibliography

