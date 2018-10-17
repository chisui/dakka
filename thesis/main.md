---
title: "Dakka: A dependently typed Actor framework for Haskell" 
author: Philipp Dargel
tags:
- Bachelor thesis
- Haskell
- Actor
- Dependent types
---

# Introduction

The goal of this thesis is to create an Actor framework, similar to Akka for Haskell. Haskell gives us many tools in its typesystem that together with Haskells purely functional nature enables us to formulate more strict constraints on actor systems. To formulate these constraints I will leverage some of Haskells dependent typing features. Another focus of the thesis is the testability of code written using the created framework.

I will show that leveraging Haskells advantages can be used to create an akka like actor framework that enables the user to express many constraints inside the typesystem itself that have to be done through documentation in akka. I will also show that exessive usage of the typesystem has some downsides that mostly relate to the maturity of Haskells dependent typing features. 

## Goals

I want to create an actor framework for Haskell that leverages the typesystem to provide safety where possible. The main issue where the typesystem can be helpful is by ensuring that only messages can be sent that can be handled by the receiving actor. It should ideally be possible for the user to add further constraints on messages and actors or other parts of the system.

Runtime components of this actor framework should be serializable if at all possible to provide. Serializeability is very desirable since it aids debugging, auditing, distribution and resilience. Debugging and auditing are aided since we could store relevant parts of the system to further review them. If we can store the state of the system we can also recover by simply restoring a previous system state or parts of it. These states could then also be sent to different processes or machines to migrate actors from one node to another.

## Result

# Fundamentals

## Actor Model

The Actor Model is a way of modeling concurrent computation where the primitive of computation is called an actor. A finite set of Actors that can communicate with each other is an Actor System. Actors can be sent messages and are characterized by the way they respond to these Messages. In Response to a message an Actor may:

1. Send a finite number of messages to other actors inside the same Actor System.
2. Add a finite number of new Actors to the Actor System.
3. Designate the behavior to be used for the next message it receives.

The Actor Model keeps these definitions very abstract. As a result of that aspect like identifying actors inside an Actor System and message ordering become implementation details.

## Akka

Akka is an implementation of the Actor Model written in Scala for the JVM. In akka some design decisions where made while implementing the Actor Model that turned out to be very useful. 

In Akka Actors are represented as classes that extend a common base class. When an actor is created a new instance of the actors class is created. The actor classs' constructor may require additional arguments. Constructor arguments have to be supplied when an actor is created. Actor classes have to provide an initial `revice` property which represents the actors inital behavior. The type of the recieve property is `PartialFunction[Any, Unit]` which means it's a possibly partial function that takes arguments of any type and returns a unit. An actor class may have fields which represent internal state. In addition to fields there is the `become` method which provides a way to switch the behavior of the current actor. Inside of its behavior the actor has access to a reference to itself as well as to the sender of the currently handled message. Inside an actor system messages of any type can be sent to any reference. There is a special message called `PoisonPill` which will terminate an actor when recieved. When an actor terminates it's designated supervisor is notified. Normally an actors supervisor is the actor that created it.[@Akka-docu]

In addition to these foundational actors Akka provides more control and features for actors like control over actors mailboxes[@Akka-docu-mailboxes], message routing[@Akka-docu-routing], clustering of Actor Systems[@Akka-docu-cluster] and more.

The way Akka is implemented lets it differ from the traditional Actor Model in some cases and extend it:

- Actors have two kinds of state: The internal state of the Actor class instance and the current `Recieve` behavior.
- A strict order on messages is enforced. For every pair of actors in the Actor System it is ensured that messages from one of those Actors to the other are handled in the same order they where sent. A notable exception to this is the `Kill` message which terminates an actor as soon as possible.
- Actors are named when they are created.
- Each actor has access to the current actor system via the `context` property. This gives any actor access to every other actor in the current actor system. Actors can be enumerated or searched for by path.
- When an actor terminates a message is sent to it's supervisor(s).
- Since Scala isn't a pure language actors can perform arbitrary operations in response to their behavior.
- Akka expects messages to be immutable.

There is also an alternative package to the described actor base package which adds type information to actors [@Akka-docu-typed]. The main differences betweene those two packages is that actor references are parametrized by the type of message that the defining actor may handle and actors have to define what kind of message they may recieve.

## Cloud Haskell

Cloud Haskell is described by its authors as a platform for Erlang-style concurrent and distributed programming in Haskell.

Since Erlang-style concurrency is implemented using the actor model Cloud Haskell already provides a full fledged actor framework for Haskell. In addition there are rich facilities to create distributed Haskell system. It doesn't make creating distributed systems in Haskell easy but is capable of performing the heavy lifting.

Unfortunately Cloud Haskell has to be somewhat opinionated since some features it provides wouldn't be possible otherwise. The biggest problem is the fact that Haskell does not provide a way to serialize functions at all. Cloud Haskell solves this through the `distributed-static` package which requires some restrictions in the way functions are defined to work.

## Dependent Typing

A dependent type is a type that depends on a value. Dependent types are a way to express relationships between values inside of a typesystem. The canonic example for dependent types is a length indexed vector. A length indexed vector is a list which length is derivable from its type. This can be defined as a Haskell GADT:

```haskell
data Vec (l :: Nat) (a :: *) where
    VNil  :: Vec 0 a
    VCons :: a -> Vec l a -> Vec (l + 1) a
```

Where `Nat` is a kind that represents positive integers as types. This example illustrates one of the core properties of dependent types: Values and types are interchangeable, that means `0` and `l + 1` are types. In Haskell this behavior can be enabled using Language extensions.  

In Haskell types and values are fundamentally different from each other. For dependent typing to be possible though there has to be a way to convert between types and values. To convert some values to types the `DataKinds` language extension was introduced. `DataKinds` allows data types to be promoted to kinds and their value constructors to types. The type equivalent to functions are type families. Haskell itself doesn't provide a mechanism to promote functions to type families though. The *singletons* library provides a way to promote functions as well as other facilities helpful for dependent typing. Since I ended up not using the *singletons* library I wont go into detail describing it here.  

## Haskell Language features 

Modern Haskell development involves many language features that are not present in the base language of *Haskell2010*. These features have to explicitly be enabled by enabling language extensions. Especially working with dependent types and using more advanced features of Haskells typesystem require many of these language extensions. Language extensions are enabled using `LANGUAGE` pragmas at the beginning of the file for which the extension should be enabled. 

- `DataKinds`: Allows types to be promoted to kinds and values to types. 
- `TypeFamilies`: Adds the ability to define type and data families. A type family can be thought of as a function on types.
- `PolyKinds`: Allows mixing different kinds. For example `k` in `l :: [k]` could normally only be of kind `*` but with `PolyKinds` it may be any kind.

These extensions are the foundation for dependent typing in Haskell. This enables the definition of `not` on types of kind `Bool`:

```haskell
type family Not (a :: Bool) :: Bool where
    Not 'True  = 'False
    Not 'False = 'True
```

Or even `elem`:

```haskell
type family Elem (e :: k) (l :: [k]) :: Bool where
    Elem e (e ': as) = 'True
    Elem e (a ': as) = Elem e as
    Elem e '[]       = 'False
```

### Heterogenous collections

Another example of the usage of some of these extensions are heterogeneous lists. That is lists that can hold values of different types at once. This can be achieved by defining a GADT `HList` that is parametrized by a list of types such that each element of `HList` has a corresponding entry in the list of types:

```haskell
data HList (l :: [*]) where
    HNil  :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)
infixr 5 `HCons`
```

With this we can now create Lists with where each element is of a different type:

```haskell
l :: HList '[Int, String, Bool]
l = 42 `HCons` "Hello World" `HCons` False `HCons` HNil
```

It is also possible to create a lookup function for elements of a given type that is only defined if the list contains an element of that type:

```haskell
class HElem e (l :: [*]) where
    hElem :: HList l -> e

instance {-# Overlaps #-} HElem e (e ': as) where
    hElem (HCons e _) = e
instance {-# Overlappable #-} HElem e as => HElem e (a ': as) where
    hElem (HCons _ as) = hElem as
```

Unlike the previous example here a type class is used instead of a type family. Matching rules differ between type families and type classes. Type families allow Non-Linear Patterns, that is the same variable may occure multiple times inside of the pattern, but type classes don't. Type classes are matched exclusively by structure. As a result both instance declarations of `HElem` look the same to compiler. Constraints are only checked after the compiler already commited to a given declaration. In this context `HElem e (e ': as)` is equvalent to `(e ~ a) => HElem e (a ': as)`. To prioritize which instance declaration will be chosen by the compiler the instances have to be annotated with overlapping instance pragmas.

### Heterogeneous Maps

A heterogeneous map may hold valued of different types at once. A values type is determined by the type of the key it is associated with. The easiest way to associate a value type with a key is to parametrize the key by the values type. The map itself is parametrized by the type of key used. A lookup function may then have the signature `lookup :: k v -> HMap k -> Maybe v` and `insert :: k v -> v -> HMap k -> HMap k`.

There are ways to implement a completly typesafe variant of `HMap`, but if there is no way of manipulating the map directly it is safe to use `unsafeCoerce` as long as the API is safe.

The base for this `HMap` will be a standard `Data.Map.Map`. To be able to use that map both keys and values have to be of a single type though. This can be achieved by creating custom `Key` and `Elem` types that capture and hide the concrete value type.

```haskell
data Key k where
    Key :: k a -> Key k

data Elem where
    Elem :: a -> Elem

newtype HMap k = HMap (Map (Key k) Elem)
    deriving Eq
```

To be able to use `Key` and `Elem` as key and value of `Map` `Key` has to implement `Ord`. Additionally we need equality on `HMap` for which both `Key` and `Elem` have to implement `Eq`.

To implement either `Eq` or `Ord` it is necessary to have an instance `Ord (k a)` for all `a`. Unfortunately it isn't possble to use the `forall` keyword in the context of instance declrations (yet [@hku-qantified-clas-constraints]). A work around until `GHC 8.6` is to capture all commonly used classes inside of the `Key` and `Elem` constructors.

```haskell
data Key k where
    Key :: (Typeable (k a), Ord (k a), Show (k a)) 
        => k a -> Key k

instance Show (Key k) where
    showsPrec d (Key k) = showsPrec d k

instance Eq (Key k) where
    Key a == Key b = Just a == typeRep b

instance Ord (Key k) where
    -- first order by type then, if type are the same use Ord
    Key a `compare` Key b = typeRep [a] `compare` typeRep [b]
                         <> Just a      `compare` cast b
```
### Typeable

In the process of compiling Haskell all type information is removed since the aren't needed at runtime. Type information may be useful at runtime sometimes. If a type is hidden via existential quantification it may be useful to be able to get a `String` representation of the captured type for debug and/or `Show` purposes. Without some way of retrieving type information at runtime it would also be impossible to define an `Eq` instance for data types using existential quantification.

Runtime type information is provided by `Data.Typeable` in Haskell2010. The type class `Typeable` provides a single function `typeRep# :: TypeRep a` where `TypeRep a` is a representation of the type `a`. `typeRep#` and `TypeRep a` are only used internally though. The module `Data.Typeable` exports ways to leverage this functionallity. GHC will derive an instance of `Data.Typeable` for every data type, type class and promoted data constructors automatically[@GHC-typeable]. Manually defining an instance of `Data.Typeable` will cause an error to ensure that the type representation is valid. 

#### Showing a type

Since `TypeRep` implements `Show` we can print any type at runtime. The `Show` implementation of `TypeRep` doesn't produce output that is equivalent to the way types are represented in haskell error messages though. This missmatch is partly due to the fact that there is no way to represent type aliases using `TypeRep` and some issues with the `Show` implementation itself[@trac-14341]. 

```haskell
showsType :: forall a. Typeable a => ShowS
showsType = showString "<<"
          . shows (typeRep (Proxy @a))
          . showString ">>"
```

#### Dynamic values and type casting

`Typeable` enables the creation of Dynamic values in Haskell. To represent a dynamic value, all we have to do is capture the `Typeable` instance of the given type. Dynamic values are implemented by `Data.Dynmaic` in `base`. To construct a dynamic value `toDyn :: Typeable a => a -> Dynamic` is used. To extract a value `fromDynamic :: Typeable a => Dynmaic -> Maybe a` which only returns a value if the expected type `a` is the same as the captured one. Data extraction is only possible because there are runtime type representations that can be compared.

In the same way values can be extracted from dynamic values it is possible to define a way to conditionally cast a value of one type to another for as long as those two types are the same where it is only known at runtime if that is the case:

```haskell
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
```

# Implementation

## Overview

The API is designed to be close to the API of Akka where appropriate. That means an actors behavior is model by a function from a message to an action. An actors action is a monad where all interactions with other actors and the actor system itself are functions that produce values in that monad.

To be able to perfom any type level computations on actors and actor systems there has to be some way of identifying specific kinds of actors by type. This is done requireing actors to implement a typeclass `Actor a` where `a` is the type we can use do identify actors by. This class has a single function called `behavior` which describes the behavior of the actor. What kind of messages an actor can handle and what kind of actors it may create in response has to be encoded in some way as well. 

The monad which models an actors action is also a typeclass that has roughly the form `Mondad m => ActorContext m`. This `ActorContext` is an *mtl* style monad class which makes it possible to have different implementations of actor systems at once. This makes it possible for example to create one implementation that is meant for testing actors and another one that actually performs these actions inside of a distributed actor system. It also makes it possible to use something else then *cloud-haskell* as a backend without having to rewrite any actor implementations.

## Actor

Since akka is not written in a pure functional language each actor can also invoke any other piece of code. This implicit capability is very useful for defining real world systems. So we have to provide a way to do something like this as well if we want to use this framework in a real world situation. Invoking any piece of code also includes managing the actor system itself. For example stopping it all together, which also turns out to be very useful. 

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

To be able to send these messages around in a distributed system we have to ensure that we can send them around. They have to essentially fulfill the same constraints as the actor type itself. For this we create a constraint type alias (possible through the language extension `ConstraintKinds`):

```haskell
type RichData a = (Show a, Binary a)
```

Now the class header can be changed to:

```haskell
class (RichData a, RichData (Message a)) => Actor a where
```

Instead of a constraint type alias we could also have used a new class and provided a single `instance (Show a, Binary a) => RichData a`. This would allow `RichData` to be partially applied. There is currently no need to do this though.

Next we have to define a way for actors to handle Messages.

```haskell
  behavior :: Message a -> ActorContext ()
```

`ActorContext` will be a class that provides the actor with a way to perform its actions.

Additionally we have to provide a start state the actor has when it is first created:

```haskell
  startState :: a
  default startState :: Monoid a => a
  startState = mempty
```

## ActorContext

We need a way for actors to perform their actor operations. To recall actors may

1. Send a finite number of messages to other actors.
2. Create a finite number of new actors.
3. Designate the behavior to be used for the next message it receives. In other words change their internal state.

The most straight forward way to implement these actions would be to use a monad transformer for each action. Creating and sending could be modeled with `WriterT` and changing the internal state through `StateT`. The innermost monad wont be a transformer of course.

But here we encounter several issues:

1. To change the sate me must know which actors behavior we are currently describing.
2. To send a message we must ensure that the target actor can handle the message.
3. To create an actor we have to pass around some reference to the actor type of the actor to create. 

The first issue can be solved by adding the actor type to `ActorContext` as a type parameter.

The second and third are a little trickier. To be able to send a message in a type safe way we need to retain the actor type. But if we would make the actor type explicit in the `WriterT` type we would only be able to send messages to actors of that exact type. Luckily there is a way to get both. Using the language extension `ExistentialQuantification` we can capture the actor type with a constructor without exposing it. To retrieve the captured type you just have to pattern match on the constructor. We can also use this to close over the actor type in the create case. With this we can create a wrapper for a send and create actions:

```haskell
data SystemMessage
    = forall a. Actor a => Send (ActorRef a) (Message a)
    | forall a. Actor a => Create (Proxy a)
  deriving (Eq, Show)
```

`ActorRef` is some way to identify an actor inside a actor system. We will define it later

Unfortunately we can't derive `Generic` for data types that use existential quantification and thus can't get a `Binary` instance for free. But as we will later discover we do not need to serialize values of `SystemMessage` so this is fine for now.

With all this we can define `ActorContext` as follows:

```haskell
newtype ActorContext a v 
    = ActorContext (StateT a (Writer [SystemMessage]) v)
  deriving (Functor, Applicative, Monad, MonadWriter [SystemMessage], MonadState a)
```

Notice that we only need one `Writer` since we combined create and send actions into a single type. Since `ActorContext` is nothing more than the composition of several Monad transformers it is itself a monad. Using `GeneralizedNewtypeDeriving` we can derive several useful monad instances. The classes `MonadWriter` and `MonadState` are provided by the `mtl` package.

Since we added the actor type to the signature of `ActorContext` we need to change definition of `behavior` to reflect this:

```haskell
  behavior :: Message a -> ActorContext a () 
```

By deriving `MonadState` we get a variety of functions to change the actors state. The other actor actions can now be defined as functions:

### send

```haskell
send :: Actor a => ActorRef a -> Message a -> ActorContext b () 
send ref msg = tell [Send ref msg]
```

Notice that the resulting `ActorContext` doesn't have `a` as its actor type but rather some other type `b`. This is because these two types don't have to be the same one. `a` is the type of actor the message is sent to and `b` is the type of actor we are describing the behavior of.The `send` function does not have a `Actor b` constraint since this would needlessly restrict the use of the function itself. When defining an actor it is already ensured that whatever `b` is it will be an `Actor`.

We can also provide an akka-style send operator as a convenient alias for `send`:

```haskell
(!) = send
```

### create

```haskell
create' :: Actor b => Proxy b -> ActorContext a ()
create' b = tell [Create b]
```

As indicated by the `'` this version of create is not intended to be the main one. For that we define:

```haskell
create :: forall b a. Actor b => ActorContext a ()
create = create' (Proxy @b)
```

In combination with `TypeApplications` this enables us to create actors by just writing `create @TheActor` instead of the cumbersome `create' (Proxy :: Proxy TheActor)`.

### ActorRef

We need a way to reference actors inside an actor system. The most straight forward way to do this is by creating a data type to represent these references. This type also has to hold the actor type of the actor it is referring to. But how should we encode the actor reference? The simplest way would be to give each actor some kind of identifier and just store the identifier:

```haskell
newtype ActorRef a = ActorRef ActorId
```

References of this kind can't be be created by the user since you shouldn't be able to associate any `ActorId` with any actor type, since there is no way of verifying at compile time that a given id is associated a given actor type. The best way to achieve this is to modify the signature of `create` to return a reference to the just created actor.

```haskell
create :: forall a. Actor a => ActorContext b (ActorRef a)
```

Additionally it would be useful for actors to have a way to get a reference to themselves. We can achieve this by adding:

```haskell
self :: ActorContext a (ActorRef a)
```

To `ActorContext`.

#### Composing references

If we assume that a reference to an actor is represented by the actors path relative to the actor system root we could in theory compose actor references or even create our own. To do this in a typesafe manner we need to know what actors an actor may create. For this we add a new type family to the `Actor` class.

```haskell
    type Creates a :: [*]
    type Creates a = '[]
```

This type family is of kind `[*]` so it's a list of all actor types this actor can create. We additionally provide a default that is the empty list. So if we don't override the `Creates` type family for a specific actor we assume that this actor does not create any other actors.

We can now also use this typefamily to enforce this assumption on the `create'` and `create` functions.

```haskell
create' :: (Actor b, Elem b (Creates a)) => Proxy b -> ActorContext a ()
```

Where `Elem` is a typefamily of kind `k -> [k] -> Constraint` that works the same as `elem` only on the type level.

```haskell
type family Elem (e :: k) (l :: [k]) :: Constraint where
    Elem e (e ': as) = ()
    Elem e (a ': as) = Elem e as
```

There are three things to note with this type family:

1. It is partial. It has no pattern for the empty list. Since it's kind is `Constraint` this means the constraint isn't met if we would enter that case either explicitly or through recursion.
2. The first pattern of `Elem` is non-linear. That means that a variable appears twice. `e` appears as the first parameter and as the first element in the list. This is only permitted on type families in Haskell. Without this feature it would be quite hard to define this type family at all.
3. We leverage that n-tuples of `Constraints` are `Constraints` themselves. In this case `()` can be seen as an 0-tuple and thus equates to `Constraint` that always holds.

The `Creates` typefamily is incredibly useful for anything we want to do that concerns the hierarchy of the typesystem. For example we could ensure that all actors in a given actor system fulfill a certain constraint. 

```haskell
type family AllActorsImplement (c :: * -> Constraint) (a :: *) :: Constraint where
    AllActorsImplement c a = (c a, AllActorsImplementHelper c (Creates a))
type family AllActorsImplementHelper (c :: * -> Constraint) (as :: [*]) :: Constraint where
    AllActorsImplementHelper c '[]       = ()
    AllActorsImplementHelper c (a ': as) = (AllActorsImplement c a, AllActorsImplementHelper c as)
```

We can also enumerate all actor types in a given actor system.

What we can't do unfortunately is create a type of kind `Data.Tree` that represents the whole actor system since it may be infinite. The following example shows this.

```haskell
data A = A
instance Actor A where
    type Creates A = '[B]
    ...

data B = B
instance Actor B where
    type Creates B = '[A]
    ...
```

The type for an actor system that starts with `A` would have to be `'Node A '[Node B '[Node A '[...]]]`. What we can represent as a type though is any finite path inside this tree.

Since any running actor system has to be finite we can use the fact that we can represent finite paths inside an actor system for our actor references. We can parametrize our actor references by the path of the actor that it refers to.

Unfortunately creating references yourself isn't as useful as one might expect. The actor type is not sufficient to refere to a given actor. Since an actor may create multiple actors of the same type you also need a way to differenciate between them to reference them directly. The easiest way would be to order created actors by creation time and use an index inside the resulting list. There are two problems with this approach though. Firstly we lose some typesafty since we can now construct actor references to actors that we can not confirm that they exist at compile time. Secondly this index would not be unambiguous since an older actor may die and thus an index inside the list of child actors would point to the wrong actor. We could take the possibility of actors dying into account which would result in essentially giving each immidiate child actor an uniquie identifier. When composing an actor reference then requires the knowledge of that exact identifier which is essentially the same as knowing the actor reference already.

The feature to compose actor references was removed because of these reasons. Actor references may now only be obtained from the `create` function and `self` for the current actor.

Typefamilies created for this feature are still useful though. They allow type level computation on specific groups of actors deep inside of an actor system.

#### Implementation specific references

Different implementations of `ActorContext` might want to use different datatypes to refer to actors. Since we don't provide a way for the user to create references themselves we don't have to expose the implementation of these references. 

The most obvious way to achieve this is to associate a given `ActorContext` implementation with a specific reference type. This can be done using an additional type variable on the class, a type family or a data family. Here the data family seems the best choice since it's injective. The injectivity allows us to not only know the reference type from from an `ActorContext` implementation but also the other way round.

```haskell
    data CtxRef m :: * -> *
```

Additionally we have to add some constraints to `CtxRef` since we need to be able to serialize it, equality and a way to show them would also be nice. For this we can reuse the `RichData` constraint. 

```haskell
class (RichData (CtxRef m)), ...) => ActorContext ... where
```

In our simple implementation I'm using an single `Word` as a unique identifier but we can't assume that every implementation wants to use it.

Now we have another problem though. Messages should be able to include actor references. If the type of these references now depends on the `ActorContext` implementation we need a way for messages to know this reference type. We can achieve this by adding the actor context as a parameter to the `Message` type family.

```haskell
  type Message a :: (* -> *) -> *
```

Here we come in a bind because of the way we chose to define `ActorContext` unfortunately. The problem is the functional dependency in `ActorContext a m | m -> a`. It states that we know `a` if we know `m`. This means that if we expose `m` to `Message` the message is now bound to a specific `a`. This is problematic though since we only want to expose the type of reference not the actor type of the current context to the `Message`. Doing so would bloat every signature that wants to move a message from one context to another with equivalence constraints like 

```haskell
forall a b m n. (ActorContext a m, ActorContext b n, Message a m ~ Message b n) => ...
```

This is cumbersome and adds unnecessary complexity.

What we might do instead is add the reference type itself as a parameter to `Message`. This alleviates the problem only a little bit though since we need the actual `ActorContext` type to retrieve the concrete reference type. So we would only delay the constraint dance and move it a little bit. These constraints meant many additional type parameters to types and functions that don't actually care about them. Error messages for users would also suffer.

In the end I decided to ditch the idea of `ActorContext` implementation specific reference types. And went another route.

Since actor references have to be serializable anyway we can represent them by a `ByteString`.

```haskell
newtype ActorRef a = ActorRef ByteString
```

This might go a little against our ideal that we want to keep the code as typesafe as possible but it's not as bad as you might think. Firstly other datatypes that might have taken the place of `ByteString` wouldn't be any safer. We can still keep the user from being able to create references by themselves by not exporting the `ActorRef` constructor. We could expose it to `ActorContext` implementers through an internal package.

#### Sending references 

A core feature that is necessary for an actor system to effectively communicate is the ability to send actor references as messages to other actors.

The most trivial case would be that the message to actor is an actor reference itself.

```haskell
instance Actor Sender where
    type Message Seder = ActorRef Reciever
    ...
```

This way limits the actor type of the receiver to be a single concrete type though. In particular we have to know the type of the actor (Receiver in the following) when defining the actor handling the reference (Sender in the following). So we would like this reference type to be more generic. A simple way to do this is to add a type parameter to the Sender that represents the Receiver.


```haskell
instance (Actor a, c a) => Actor (Sender a) where
    type Message (Sender a) = a
    ...
```

`c` here can take any constraint that the Receiver actor has to fulfill as well. This is more generic but we `a` still represents a concrete type at runtime. The way this is normally done in Haskell is by extracting the commonalities of all Receiver types into a typeclass and ensure that all referenced actors implement that typeclass.

```haskell
class Actor a => Reciever a
instance Reciever a => Actor (Sender a) where
    type Message (Sender a) = a
    ...
```

Although this is just a variation on the previous way since we only consolidated `c` into the `Reciever` class. Unfortunately we can't use `forall` in constraint contexts (yet; see `QuantifiedConstraints`). To get around this we can create a new message type that encapsulates the constraint like this:

```haskell
data AnswerableMessage c = forall a. (Actor a, c a) => AnswerableMessage (ActorRef a)
```

With this we can define the Sender like this:

```haskell
class Actor a => Reciever a
instance Actor Sender where
    type Message Sender = AnswerableMessage Reciever 
    ...
```

`Reciever` should not perform long running tasks though since that would provide a way to circumvent the actor model somewhat since the task would be performed in the context of the `Sender`. Ideally the class should only provide a way to construct a message the `Reciever` understands from a more generic type. We can express this with a typeclass like this:

```haskell
class Actor a => Understands m a where
    convert :: m -> Message a
```

A `Sender` may use this class like this:

```haskell
instance Actor Sender
    type Message Sender = AnswerableMessage (Understands SomeType)
    onMessage (AnswerableMessage ref) = do
        ref ! convert someType
```

Solving the problem of sending generic actor references has a huge problem though. Using existential quantification prevents `AnswerableMessage` from being serialized. Serializeability is a core requirement for messages though.

I do have an idea of how to get around this restriction but wasn't able to test it yet. To serialize arbitrary types we would need some kind of sum-type where each constructor corresponds with one concrete type. Since we can enumerate every actor type of actors inside a given actor system from the root actor we could use this to create a dynamic union type. An example of a dynamic union type would be `Data.OpenUnion` from the `freer-simple` package. To construct this type we need a reference to the root actor though so that type has to be exposed to the actor type in some way, either as an additional type parameter to the `Actor` class or to the `Message` typefamily. Adding a type parameter to `Actor` or `Message` requires would require rewriting ab big chunk of the codebase though. Sending `ActorRef` values directly is the only possible way for now.

### Flexibility and Effects

By defining `ActorContext` as a datatype we force any environment to use exactly this datatype. This is problematic since actors now can only perform their three actor actions. `ActorContext` isn't flexible enough to express anything else. We could change the definition of `ActorContext` to be a monad transformer over `IO` and provide a `MonadIO` instance. This would defeat our goal to be able to reason about actors though since we could now perform any `IO` we wanted.

Luckily Haskells typesystem is expressive enough to solve this problem. Due to this expressiveness there is a myriad of different solutions for this problem though. Not all of them are viable of course. We will take a look at two approaches that integrate well into existing programming paradigms used in Haskell and other functional languages.

Both approaches involve associating what additional action an actor can take with the `Actor` instance definition. This is done by creating another associated typefamily in `Actor`. The value of this typefamily will be a list of types that identify what additional actions can be performed. What this type will be depends on the chosen approach. The list in this case will be an actual Haskell list but promoted to a kind. This is possible through the `DataKinds` extension. 

#### mtl style monad classes

In this approach we use mtl style monad classes to communicate additional capabilities of the actor. This is done by turning `ActorContext` into a class itself where `create` and `send` are class members and `MonadState a` is a superclass.

The associated typefamily will look like this:

```haskell
  type Capabillities a :: [(* -> *) -> Constraint]
  type Capabillities a = '[]
```

With this the signature of `behavior` will change to:

```haskell
  behavior :: (ActorContext ctx, ImplementsAll (ctx a) (Capabillities a)) => Message a -> ctx a ()
```

Where `ImplementsAll` is a typefamily of kind `Constraint` that checks that the concrete context class fulfills all constraints in the given list:

```haskell
type family ImplementsAll (a :: k) (c :: [k -> Constraint]) :: Constraint where
    ImplementsAll a (c ': cs) = (c a, ImplementsAll a cs)
    ImplementsAll a '[]       = ()
```

To be able to run the behavior of a specific actor the chosen `ActorContext` implementation has to also implement all monad classes listed in `Capabillities`.

```haskell
newtype SomeActor = SomeActor ()
  deriving (Eq, Show, Generic, Binary, Monoid)
instance Actor SomeActor where
    type Capabillities SomeActor = '[MonadIO]
    behavior () = do
        liftIO $ putStrLn "we can do IO action now"
```

Since `MonadIO` is in the list of capabilities we can use its `liftIO` function to perform arbitrary `IO` actions inside the `ActorContext`.

`MonadIO` may be a bad example though since it exposes to much power to the user. What we would need here is a set of more fine grain monad classes that each only provide access to a limited set of IO operations. Examples would be Things like a network access monad class, file system class, logging class, etc. These would be useful even outside of this actor framework.

#### the Eff monad

The `Eff` monad as described in the `freer`, `freer-effects` and `freer-simple` packages is a free monad that provides an alternative way to monad classes and monad transformers to combine different effects into a single monad.

A free monad in category theory is the simplest way to turn a functor into a monad. In other words it's the most basic construct for that the monad laws hold given a functor. The definition of a free monad involves a hefty portion of category theory. We will only focus on the aspect that a free monad provides a way to describe monadic operations without providing an interpretations immediately. Instead the there can be multiple ways to interpret these operations. 

When using the `Eff` monad there is only one monadic operation:

```haskell
send :: Member eff effs => eff a -> Eff effs a
```

`effs` has the kind `[* -> *]` and `Member` checks that `eff` is an element of `effs`. Every `eff` describes a set of effects. We can describe the actor operations with a GADT that can be used as effects in `Eff`:

```haskell
data ActorEff a v where
    Send   :: Actor b => ActorRef b -> Message b -> ActorEff a ()
    Create :: Actor b => Proxy b -> ActorEff a ()
    Become :: a -> ActorEff a () 
```

With this we can define the functions:

```haskell
send :: (Member (ActorEff a) effs, Actor b) => ActorRef b -> Message b -> Eff effs ()
send ref msg = Freer.send (Send ref msg)

create :: forall b a effs. (Member (ActorEff a), Actor b) => Eff effs ()
create = Freer.send $ Create (Proxy @b)

become :: Member (ActorEff a) effs => a -> Eff effs ()
become = Freer.send . Become
```

We could also define these operations without a new datatype using the predefined effects for `State` and `Writer`:

```haskell
send :: (Member (Writer [SystemMessage]) effs, Actor b) => ActorRef b -> Message b -> Eff effs ()
send ref msg = tell (Send ref msg) 

create :: forall b a effs. (Member (Writer [SystemMessage]), Actor b) => Eff effs ()
create = tell $ Create (Proxy @b)
```

`become` does not need a corresponding function in this case since `State` already defines everything we need.

## Testing

One of the goals of the actor framework is testability of actors written in the framework. The main way that testability is achieved is by implementing a special `ActorContext` that provides a way to execute an actors behavior in an controlled environment. The name of this `ActorContext` is `MockActorContext`. `MockActorContext` has to provide implementations for `create`, `send` and `MonadState`. Additionally we need a way to execute a `MockActorContext`. One way to define `MockActorContext` is using monad transformers in conjunction with `GeneralizedNewtypeDeriving`.

```haskell
newtype MockActorContext a v = MockActorContext
    ( ReaderT (ActorRef a) 
        ( StateT CtxState 
            (Writer [SystemMessage])
        ) v
    )
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [SystemMessage]
    , MonadReader (ActorRef a)
    )
```

Where `CtxState` is used to keep track of actor instances that currently are in known to the context.

```haskell
data CtxState = CtxState
    { nextId :: Word
    , states :: HMap ActorRef
    }
  deriving 
    ( Show
    , Eq
    )
```

`MonadState` is a prerequisite for `ActorContext` so an instance of that has to be provided.

```haskell
instance Actor a => MonadState a (MockActorContext a) where
    get = do
        ref <- ask
        MockActorContext . gets $ ctxLookup ref
    put a = do
        ref <- ask
        MockActorContext $ do
            CtxState i m <- get
            let m' = HMap.hInsert ref a m 
            put $ CtxState i m'
```

With this the actual definition of `ActorContext` for `MockActorContext` is pretty short.

```haskell
instance (Actor a, MockActorContext a `CanRunAll` a) => ActorContext a (MockActorContext a) where

    self = ask

    create' _ = do
      ref <- MockActorContext ctxCreateActor
      tell [Left $ Creates ref]
      pure ref

    p ! m = tell [Right $ Send p m]
```

To execute a single `MockActorContext` action all monad transformer actions have to be executed. It doesn't make sense though to export this capability directly since `CtxState` should not be visible to the user. So exported variants on the core running function construct `CtxState` values themselves.

```haskell
runMockInternal :: forall a v. Actor a => MockActorContext a v -> ActorRef a -> CtxState -> ((v, CtxState), [SystemMessage])
runMockInternal (MockActorContext ctx) ref = runWriter . runStateT (runReaderT ctx ref)
```

## executing in a destributed environment

When executing an actor inside a destributed environment you have to take care of message passing and the actual concurrent execution. This is a hard problem. Luckily cloud-haskell already exists and provides exactly what is needed here. It is itself a Erlang-style actor framework. Although all it's messages are untyped and every actor has access to `IO`. This enables us to execute the previously defined typed actors ontop of it.

As with the mock case the cetral entry point will be the `ActorContext`. All actions in cloud-haskell are inside of the `Process` monad. So we need to keep track of the actors state and access to the `Process` monad. This can be achieved using a `newtype` wrapper around `StateT` transformer of `Process`:

```haskell
newtype DistributedActorContext a v
    = DistributedActorContext
        { runDAC :: StateT a Process v
        }
  deriving (Functor, Applicative, Monad, MonadState a, MonadIO)
```

`GeneralizedNewtypeDeriving` is used to derive the normaly not derivable instances like `MonadIO`. `ActorContext` has to be implemented manually.

```haskell
instance (DistributedActorContext a `A.CanRunAll` a) => A.ActorContext a (DistributedActorContext a) where
    self = A.ActorRef . encode <$> liftProcess getSelfPid
    (A.ActorRef pid) ! m = liftProcess $ send (decode pid) m

    create' a = liftProcess $ do
        nid <- processNodeId <$> getSelfPid
        pid <- spawn nid (staticRunActor a)
        return . A.ActorRef . encode $ pid
```

All we have to do to implment `self` and `(!)` is to wrap/unwrap the process id in an `ActorRef` and use the functions that `Process` gives us.
Notice that `create'` uses `staticRunActor` instead of `runActor`. More on this in the next section

Executing an `Actor` in this context now means dispatching a `Created` signal and continously polling for messages.

```haskell
runActor :: forall a proxy. (A.Actor a, DistributedActorContext a `A.CanRunAll` a) => proxy a -> Process ()
runActor _ = void . runStateT (runDAC (initActor *> forever awaitMessage)) $ A.startState @a
  where
    initActor = A.behavior . Left $ A.Created
    awaitMessage = A.behavior . Right =<< liftProcess (expect @(A.Message a))
```

### Creating Actors

Creating an `Actor` means spawning a new `Process` that executes `runActor` for that specific actor type. The problem here is that the instruction on what the `Process` should do has to be serializable. Since functions are not Serializeability in Haskell cloud-haskell provides a workaround with the *distirbuted-static* package.

It provides a way to serialize references to values and functions that are known at compiletime and compose these. This is done using a heterogeneous map that has to be manually populated with all static values that the program may encounter while running. Unfortunately there is no way to register polymorphic functions like `runActor` this way. Luckily there is a way to enumerate all actor types that exist in a given actor system and could register a version of `runActor` for each one. I wasn't able to do this though for time reasons. As the hirarchy of the actor system, described by the `Creates` typefamily is potentially infinite, the registration would have to perform cycle detection on the type level.

As a result dakka currently hasn't the capability to run a full distributed actor system.

# Results

Although I do not have a runnable distributed system there are usable results in here. 

## Actor framework

I demonstrated that it is possible to create an actor framework in Haskell that is capable of expressing many constraints about it's hierarchy and the capabilities of the actors in it using the typesystem. 

## Dependent types in Haskell

Dependent types are a powerful tool in Haskell. Unfortunately their usabillity is somewhat limited since they aren't supported natively. The lack of native support doesn't make it's use impossible but prevelent usage cumbersome. Promotion of values to types and demotion from types to values has to be done manually. For promotion and demotion there is the library *singletons*, which is written by the author of the `TypeInType` language extension, that is central to dependent types in Haskell. The *singletons* way to promote functions, by generating corresponding typefamilies for them but debugging these generated typefamilies is extremely hard. When there is an error in the types GHC either just has the names of the generated typefamilies, that are not very informative or there is just a statement that two types don't unify without a information on how GHC got those two types. These debugging problems make the usage of the *singletons* library and dependent types in general tedious for library authors. The effects are even worse for users of libraries that heavily leverage *singletons* that do not have a good understanding of that library or dependent types since they might get the same kind of cryptic errors unless the library author takes extra steps and exports special interface types that produce more readable errors. 

As a result of this I opted to reduce the usage of dependent types in my code and do without the *singletons* library alltogether. Even though they are not dependent types many of the more advanced type-level-computation features Haskell provides were extremely useful 

## Cloud Haskell

Since most of the effort went into creating a typed interface rather then the actual execution of it I can't comment on how cloud haskell actually performes as a backend for the created interface.

# Bibliography
