
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

I suspect that writing a converter class for responses and having a Message type that embedds a reference to the sender that is constrained by that converter class will be a common pattern. It may make sense to provide an abstraction for this type of pattern.

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

We want to be able to have the complete tree of the actorsystem be represented in a type. Each reference that is used to send messages to actors should be represent a path in that tree. For this the actor or the ActorContext to be more precise has to have some knowledge about the actors position in the Tree. We also would like to be able to reuse Actors at multiple Position in the Tree and maybe also in different Actor systems alltogether. It makes the most sense to only expose the bare minimum of Information about the tree to the actor to ensure decoupling. One approach is to add another type variable that represents the Tree. This variable is not bound specifically by the Actor of ActorContext but the Actor may add Constraints to this variable that either provide the Actor with specific knouwledge about it's ActorSystem and it's position inside of it or constrain where inside the Tree The actor may Occure.

One such Constraint to the tree already is in present with the `Creates` typefamily. In fact just by itself this typefamily defines the Tree of an Actor system. But it only provides downward information. If an actor needs upward information of it's parents that can't be expressed in this way.

