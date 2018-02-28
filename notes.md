
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
