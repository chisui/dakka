
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


