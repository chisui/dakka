
# 18-02-16

- Actor system als typ representieren
- Typ sollte Elter, Kind beziehung abbilden.
- Also Baum
- Erster Ansatz: Typ direkt represntieren.
- - Verworfen da nicht genug Informationen representierbar
- Zweiter Ansatz: Singletons
- - Baum `data Tree a where (:->) a -> [Tree a]` promoten
- - Singleton Baum `STree (t :: Tree a)`
- - Informationen representierbar
- - Keine Daten in `STree` speicherbar da Singletontyp.
- Dritter Ansatz: Singleton mit Werten
- - Sehr ähnlich zu Singletontyp.
- - Typefamilies für dependent type wiederverwendbar
- - Typen im ersen Entwurf sehr konkret. Allerdings schwer zu generalisieren ohne große Komplexitätssteigerung.

# 18-02-28

- `ActorContext` sollte eine Typklasse sein damit Aktoren einfacher getestet werden können und Die Interna des Aktor systems abstrahiert sind.
- `ActorContext` sollte monadisch sein.
- Ist abhängig von Aktortyp sowie monadischem Wert. Also `class ActorContext (m :: * -> * -> *)`
- `Monad (m a)` muss für alle `m` und `a` gelten. Leider lässt sich dieses Constraint nicht ohne weiteres formuliere (https://homepages.inf.ed.ac.uk/wadler/papers/quantcc/quantcc.pdf). Daher hat jede Funktion von `ActorContext` ein explizites `Monad (m a)` Constraint.

- Alternative Singnatur von `ActorContext` mit Functional Dependencies, orientiert sich an `MonadState` des `mtl` packages: `class ActorContext a m | m -> a`. Damit wird `Monad (m a)` Constraint zu `Monad m`.
- `Actor a` Constraint wird so ebenfalls auf Klassenebene gehoben.

- Um einen Aktor zu erzeugen muss ein Startzustand übergeben werden: `create :: (Actor b, Elem b Creates a) => b -> m (Path b)`.
- `Elem` ist hier nicht das `singletons` Constraint sondern eine eigene Klasse mit eigener Typefamily:
  ```haskell
  class Elem (e :: *) (l :: [*])
  instance (ElemF e l ~ 'True) => Elem e l
  type family ElemF (e :: *) (l :: [*]) where
      ElemF e (e ': as) = 'True
      ElemF e (a ': as) = ElemF e as
  ```
  Die Typefamily `ElemF` ist hier absichtlich partiell um bessere Fehlermeldungen zu erzeugen. Gäb es einen `ElemF e '[] = 'False` Fall wäre die die Fehlermeldung: `couldn't match type ´'False´ with 'True´`. Das selbe würde passieren wenn `Elem` aus `singletons` verwendet würde. So ist die Fehlermeldung `Couldn't match type ´ElemF <Actor> <CreatesList>´ with ´'True´`.
- Die Fehlermeldung könnte noch weiter verbessert werden indem die geschlosse Typefamily in mehrere, überlappende Instanzen übersetzt würde. Das ist allerdings nicht möglich, da Es dann mehrere declatationen für `Elem e (a ': as)` gäbe, die sich ausschließlich in ihren Constraints unterscheiden.
- Mit der `ConstraintKinds` Erweiterung ist es möglich partielle Typefamilien als Constraint zu verwenden.
- Mit der `TypeOperators` Erweiterung können infix Typoperatoren definiert werden. Diese müssen mit einem `:` beginnen. Zusammen mit `ConstraintKinds` lässt sich `Elem` so ausdrücken:
  ```
  type family (e :: k) :∈ (l :: [k]) :: Constraint where
      e :∈ (e ': as) = ()
      e :∈ (a ': as) = e :∈ as
  ```
  Fehlermeldungen sehen damit so aus: `Could not deduce: <Actor> :∈ <CreatesList>`.

## Actor

```haskell
type Behavior a = forall m. ActorContext a m => Message a -> m ()
type RichData a = (Show a, Eq a, Typeable a)
class (RichData a, RichData (Message a), Actor `ImplementedByAll` Creates a) => Actor (a :: *) where
    type Creates a :: [*]
    type Creates a = '[]

    type Message a :: *
    behavior :: ActorContext a m => Behavior a
```

- An `Actor` is represented by the type of its state.
- Additionally an `Actor` has to associated types:
- - The types of immediat child Actors it may create, which is none by default. This type is an associated typefamily so it may have a default.
- - The type of message it may consume. This type could have been part of the class signature and bound by a functional dependency but expressing it as an associated typefamily makes for cleaner Constraints: `Actor a` vs. `Actor a m` where `m` wont may be unused.
- Both the actor state and the message type have to implement some basic classes to enable debugging.
- All members of the `Creates` List have to be `Actors`.

### Behavior

The behavior of an `Actor` can cause three things:

1. Change the interal state of the `Actor`
2. Send messages to other actors
3. Create new actors

All these effects can be expressed using Monad transformers. The state change can be represented by `StateT` and sending messages and creating actors by `WriterT` respectively. We also need to encapsulate sideeffects in some way so we are pretty much forced to create a monadic structure. Lets call this combined Monad `ActorContext` for now. One sensible way of defining `ActorContext` might be:

```haskell
data SystemMessage
    = forall a. Actor a => Create (Proxy a)
    | forall a. Actor a => Send (Path a) (Message a)
type ActorContext a = (Actor a) => StateT a (WriterT [SystemMessage] IO) 
```

Since we used monad transformers `ActorContext` has an `MonadState` instance. So we don't have to define anything to enable state changes of the actor. With this definition we can define functions for sending messages and creating actors:

```haskell
create :: (Actor a, Actor b, b :∈ Creates a) => Proxy b -> ActorContext a ()
create p = tell [Create p]

send :: (Actor a, Actor b) => Path b -> Message b -> ActorContext a ()
send p m = tell [Send p m]
```

Having a rigid occurence of `IO` in the context though is less than desireable. This means that we can't confidently test any behavior. It would be better to abstract at least the `IO` part of the context we have to change the definition of `ActorContext`. A first step could be to lift the transfomred monad to a type variable:

```haskell
type ActorContext a m = Actor a => StateT a (WriterT [SystemMessage] m)
```


### Answering messages

It's important for a functioning Actor system for actors to be able to respond to messages in a meaningful way. So we have to ensure that this is possible in our implementation supports it.

We do not want to expose a general way of determining the sender of a message, as that would impose to many unnecessary constraints on the ActorContext. These constraints should be as granular as possible to enable easy testing and reasoning about the system. So we have to encode the Sender directly into the message:

```haskell
data MyMsg = forall a. Actor a
    => MyMsg
        { sender :: Path a
        } deriving (Eq, Show, Typeable)
```

To be able to do something with the `Actor` we have to further constrain `a`. We also need a way of sending a response to the sender. The easiest way to achieve this is to create a class that maps a response to the senders message type:

```haskell
class IsMyResponse a where
  toMyResponse :: MyResponse -> a
data MyResponse = MyResponse String
data MyMsg = forall a. (Actor a, IsMyResponse (Message a))
    => MyMsg
        { sender :: Path a
        } deriving (Eq, Show, Typeable)
```

With this we can write an `Actor` that can answer messages:

```haskell
data MyActor = MyActor deriving (Eq, Show, Typeable)
instance Actor MyActor where
    type Message MyActor = MyMsg
    behavior (MyMsg sender) = do
        sender ! toMyResponse "hello"
```

I suspect that writing a converter class for responses and having a Message type that embedds a reference to the sender that is constrained by that converter class will be a common pattern. It may make sense to provide an abstraction for this type of pattern:

```haskell
class a `Injectable` b where
    inject :: a -> b
instance a `Injectable` a where inject = id
data AnswerableMsg r = forall a. (Actor a, r `Injectable` (Message a))
    => AnswerableMsg
        { sender :: Path a
        } deriving Typeable

answer :: ActorContext a m => r -> AnswerableMsg r -> m ()
answer r (AnswerableMsg sender) = sender ! inject r
```

Since messages have to satisfy the `Eq` constraint we can't add the conversion function as a field on the message itself. On the other hand you could define several standard instances for `Injectable` like `Applicative f => Injectable a (f a)`, `IsString t => Injectable String t` or `Injectable Word Integer`.

It should also be ensured that `inject` is in fact injective. This could be achieved either by using something like liquid haskell or adding a function `extract :: b -> Maybe a` with the law `extract (inject a) == Just a`.

With this boilerplate we don't need to define a custom classified message type and the `Actor` definition is reduced to:

```haskell
data MyActor = MyActor deriving (Eq, Show, Typeable)
instance Actor MyActor where
    type Message MyActor = AnswerableMsg String
    behavior = answer "hello"
```

## Additional Effects

So far `ActorContext` only allows for purely functional behaviors. If the Actors should do anything useful it has to be able to interact with the environment. For this we can further constrain the `ActorContext` so that it may provided the Actor with nessecary capabillities. For this we create the associated type family `Capabillities` on `Actor` with the kind `[(* -> *) -> Constraint]`. Furthermore we extend Behavior such that the monad type parameter has to fullfill all `Capabillity` constraints:

```haskell
type Behavior a = forall m. (ActorContext a m, m `ImplementsAll` Capabillities a) => Message a -> m ()
```

Context now has to implement all classes in `Capabillities` to be able to run the Behavior.
All types in the capabillities List should be monad transformers like `MonadIO`. Although a more granular type should be chosen to be more expressive about what the Actor actually does and to provide better testing.

After comming up with this approache I found out about [`freer-effects`](http://reasonablypolymorphic.com/dont-eff-it-up/). The presentation hsa some good points about boilerplate of mtl stye monad transformers, which is what I was using before.

I will investigate how the `Eff` approache may be used in the current implementation. My hunch is though that it'll fit perfectly.

## Combining Tree representation with actors

We want to be able to have the complete tree of the actorsystem be represented in a type. Each reference that is used to send messages to actors should be represent a path in that tree. For this the actor or the ActorContext to be more precise has to have some knowledge about the actors position in the Tree. We also would like to be able to reuse Actors at multiple Position in the Tree and maybe also in different Actor systems alltogether. It makes the most sense to only expose the bare minimum of Information about the tree to the actor to ensure decoupling. One approach is to add another type variable that represents the Tree. This variable is not bound specifically by the Actor of ActorContext but the Actor may add Constraints to this variable that either provide the Actor with specific knowledge about it's ActorSystem and it's position inside of it or constrain where inside the Tree The actor may Occure.

One such Constraint to the tree already is in present with the `Creates` typefamily. In fact just by itself this typefamily defines the Tree of an Actor system. But it only provides downward information. If an actor needs upward information of it's parents that can't be expressed in this way.

The main part that has to change is how actors are referenced. Currently This is done my a placeholder type that doesn't provide a meaningful way of actually referencing an actor:

```haskell
data ActorRef a = ActorRef Word
```

We will improve this in the following two steps.

### Make Actor Ref a Path

We want `ActorRef` to be a path where path information is present in it's type. For this we create a new datatype `Path`:

```haskell
data Path a
    = Root
    | Path a :/ a
```

`Path` is essentially a list, with the important destinction that the constructor is left recursive. This makes pushing and popping of path segemnts easier. This will come in handy when we promote `Path` to a kind.

#### Promoting Path

Now we have a type that represents paths. But the Strukture of this type is not present in it's type. The fastest way would be to use the `singletons` library to promote `Path` to a singleton type. Actors may create multiple children of the same type at runtime. So it doesn't make sense to represent that information in the type. That means that our `ActorRef` type can not be a simple singleton type. We need to embedd additional runtime information.

Rathern than relying on `singletons` to promote our type we have to do it manually, but we can use the same approach as the library:

```haskell
data IndexedPath (p :: Path *) where
    IRoot :: IndexedPath 'Root
    (://) :: IndexedPath as -> Word -> IndexedPath (as ':/ a)
```

`IndexedPath` is a `GADT` with one typevariable thats kind is `Path *`. Here we promoted `Path` to a kind. Now we can only create values of `IndexedPath` that contain their structure in their type.

Additionally there are utillity functions to for `IndexedPath`. In combination they allow path creation like this:

```haskell
>>> :t Proxy @Int </> ref @String 2
Proxy @Int </> ref @String 2
  :: IndexedPath ('Root ':/ Int ':/ [Char])
```

#### Replacing ActorRef

We change `data ActorRef a = ActorRef Word` to `type ActorRef a = IndexedPath a`. By doing this we change the meaning of `a` though. Before it represented the refered actor itself. Now it represents a path where the last segemnt is the refered actor. That means we need a way to retrieve the actor type `a :: *` from a path `p :: Path *`. This is easily achieved with a closed typefamily:

```haskell
type family Tip (p :: Path k) k where
    Tip (as ':/ a) = a
```

This typefamily is intentially partial so that `'Root` does not have a representation.

`send` can now have this signature:

```haskell
send :: Actor (Tip b) => ActorRef b -> Message (Tip b) -> m ()
```

`create` though, can not be expressed without further changes. Before the signature of `create` was:

```haskell
create :: (Actor b, b :∈ Creates a) => Proxy b -> m (ActorRef b)
```

We can't simply use the same trick as with `send`.

```haskell
create :: (Actor (Tip b), (Tip b) :∈ Creates a) => Proxy (Tip b) -> m (ActorRef b)
```

This is no valid solution for our problem. Firstly because it wont compile. `Tip` is not injective so `b` can not be infered by `Tip b`, which is absolutely ok. Beeing able to do so would mean that we could infere any path from one of it's segments. That doesn't make any sense.
Secondly we don't constrain the path in any way. Even if we could get this to compile we would be able to create actors at an arbitrary position in the actor system. That too doesn't make any sense.

The solution is to give the `ActorContext` an additional type variable `p :: Path *` that holds the actors path. To ensure that `p` always points to the correct actor we ad the constraint `a ~ Tip p` to `ActorContext`.

This way we also ensure that an actor doesn't know too much about it's position in the actor system. It only knows about the subtree beneath itself. The path up to it is completly opaque. 

Now `create` can be expressed:

```haskell
create :: (Actor b, b :∈ Creates a) => Proxy b -> m (ActorRef (p ':/ b))
```

Notice that the type of the returned actor reference now describes the current actors path followed by the type of the created actor.

### Add information about the rest of the actor system

With `ActorRef` now representing a path we made the creation of actors a little bit more declarative. The `ActorRef` that `create` now returns reflects that the last two segemnts of the path represent the current followed by the created actor. Without any information about the rest of the system though we can not do anything with that information. Furthermore it is possible to send messages to arbitrary paths as long as the tip of that path is an actor that acceps the messages type.

We would like to constrain `ActorRefs` even further such that they have to be internally consistent.  

