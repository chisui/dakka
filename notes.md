
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
    ¦ ElemF e (a ': as) = ElemF e as
  ```
  Die Typefamily `ElemF` ist hier absichtlich partiell um bessere Fehlermeldungen zu erzeugen. Gäb es einen `ElemF e '[] = 'False` Fall wäre die die Fehlermeldung: `couldn't match type ´'False´ with 'True´`. Das selbe würde passieren wenn `Elem` aus `singletons` verwendet würde. So ist die Fehlermeldung `Couldn't match type ´ElemF <Actor> <CreatesList>´ with ´'True´`.
- Die Fehlermeldung könnte noch weiter verbessert werden indem die geschlosse Typefamily in mehrere, überlappende Instanzen übersetzt würde. Das ist allerdings nicht möglich, da Es dann mehrere declatationen für `Elem e (a ': as)` gäbe, die sich ausschließlich in ihren Constraints unterscheiden.
